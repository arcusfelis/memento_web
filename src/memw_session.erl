-module(memw_session).
-behaviour(gen_server).
-include_lib("memento_web/src/memento_web.hrl").
-define(SUPERVISOR, memw_session_sup).

-record(state, {
    connection,
    register
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([ start/2
        , start_link/2
        , send/2
        , update_connection/2]).


%% ------------------------------------------------------------------
%% Gproc Function Exports
%% ------------------------------------------------------------------

-export([lookup_server/1]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Connection, Hash) ->
    gen_server:start_link(?MODULE, [Connection, Hash], []).

start(Connection, Hash) ->
    supervisor:start_child(?SUPERVISOR, [Connection, Hash]).

send(Server, Action) ->
    gen_server:cast(Server, Action).

update_connection(Server, NewConnection) ->
    gen_server:call(Server, {update_connection, NewConnection}).


%% ------------------------------------------------------------------
%% Gproc helpers
%% ------------------------------------------------------------------

register_server(SessionId) ->
    memw_gproc:register(server_name(SessionId)).

lookup_server(SessionId) ->
    memw_gproc:lookup(server_name(SessionId)).

await_server(SessionId) ->
    memw_gproc:await(server_name(SessionId)).

server_name(SessionId) ->
    {memento_web, SessionId, session}.

session_id() ->
    base64:encode(crypto:rand_bytes(64)).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Connection, Hash]) ->
    State = #state{connection=Connection},

    %% Get an unique id
    SessionId = session_id(),

    %% Send this id to the client.
    %% Then if a disconnect occures, JS part can reconnect to this process.
    Data = [{<<"session_id">>, SessionId}],

    %% Hash is a hash of an object, that will receive an operation result.
    Cmd = memw_stream_handler:fire_event(Hash, <<"sessionStarted">>, Data),
    send_command(Cmd, State),

    register_server(SessionId),

    {ok, State}.


handle_call({update_connection, NewConnection}, _From, State) ->
    State2 = State#state{
        connection=NewConnection
    },
    {reply, ok, State2}.

handle_cast(#memw_action{}=A, State) ->
    NewState = handle_action(A, State),
    {noreply, NewState};

handle_cast(#memw_event{}=E, State) ->
    NewState = handle_event(E, State),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------




handle_action(#memw_action{type = <<"registerObject">>} = A, State) ->
    #memw_action{hash = Hash, path = Path} = A,
    register_object(Path, Hash, State).


%% BasicTable
register_object([<<"memento_web">>, Type, <<"Table">>], Hash, State) ->
    async(fun() ->
        lager:info("~p Register table ~p", [Hash, Type]),
        MnesiaTableName = memw_meta:type_to_table_name(Type),
        Records = m_store:all_entries(MnesiaTableName),
        Rows = [ memw_json:record_to_json(Rec) || Rec <- Records],
        Data = [{<<"rows">>, Rows}],
        Cmd = memw_stream_handler:fire_event(Hash, <<"dataAdded">>, Data),
        send_command(Cmd, State)
        end),
    Obj = #memw_table{hash=Hash, type=Type},
    add_object(Obj, State);

%% BasicForm
register_object([<<"memento_web">>, Type, <<"Form">>], Hash, State) ->
    lager:info("~p Register form ~p", [Hash, Type]),
    Cmd = memw_stream_handler:add_listener(Hash, <<"dataRequest">>),
    send_command(Cmd, State),
    Cmd2 = memw_stream_handler:add_listener(Hash, <<"dataSubmit">>),
    send_command(Cmd2, State),
    Obj = #memw_form{hash=Hash, type=Type},
    add_object(Obj, State).


add_object(Obj, State=#state{register=R}) ->
    State#state{register=[Obj|R]}.

get_object(Hash, State=#state{register=R}) ->
    lists:keyfind(Hash, #memw_object.hash, R).


send_command(Cmd, #state{connection=Pid}) ->
    memw_stream_handler:send_command(Pid, Cmd).


%% BasicForm wants to download data
handle_event(#memw_event{type = <<"dataRequest">>, data=[_|_]} = E, State) ->
    #memw_event{hash=Hash, data=Data} = E,
    %% The target of the event
    #memw_form{type=Type} = get_object(Hash, State),
    Id = proplists:get_value(<<"id">>, Data),
    if is_integer(Id) ->
        async(fun() ->
            lager:info("~p Data request of record ~p form ~p", [Hash, Id, Type]),
            MnesiaTableName = memw_meta:type_to_table_name(Type),
            Record = m_store:get_entry_by_id(MnesiaTableName, Id),
            EncRec = memw_json:record_to_json(Record),
            Cmd = memw_stream_handler:fire_event(Hash, <<"dataLoaded">>, EncRec),
            send_command(Cmd, State)
        end),
        State
    end;

%% BasicForm wants to update data
handle_event(#memw_event{type = <<"dataSubmit">>, data=[_|_]} = E, State) ->
    %% Ready to write. More validation will be here.
    async(fun() ->
        #memw_event{hash=Hash, data=Data} = E,
        %% The target of the event
        #memw_form{type=Type} = get_object(Hash, State),
        Name = memw_meta:type_to_record_name(Type),
        Record = memw_json:json_to_record(Name, Data),
        lager:info("Decoded record: ~p", [Record]),
        m_store:write(Record),

        %% Send new version of the record to the form       
        EncRec = memw_json:record_to_json(Record),
        Cmd = memw_stream_handler:fire_event(Hash, <<"dataUpdated">>, EncRec),
        send_command(Cmd, State)
    end),
    State.


async(F) ->
    erlang:spawn_link(F).
