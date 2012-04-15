-module(memw_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).
-export([send/2]).

-export([send_command/2]).
-export([ fire_event/2
        , fire_event/3
        , add_listener/2]).

-include_lib("memento_web/src/memento_web.hrl").

-record(state, {
    hub,
    hub_mon
}).


init(_Transport, Req, _Opts, _Active) ->
    State = #state{},

    {ok, Req, State}.

json_to_record(JsonProps) ->
    Schema = proplists:get_value(<<"schema">>, JsonProps),
    lager:info("SCHEMA: ~p~n", [Schema]),
    case Schema of
    <<"auth">> -> 
        json_to_auth(JsonProps);
    <<"action">> ->
        json_to_action(JsonProps);
    <<"event">> ->
        json_to_event(JsonProps)
    end.


check_hash(Hash) when is_binary(Hash) -> Hash.


json_to_action(JsonProps) ->
    #memw_action{
        type = proplists:get_value(<<"type">>, JsonProps),
        hash = check_hash(proplists:get_value(<<"hash">>, JsonProps)),
        path = proplists:get_value(<<"path">>, JsonProps)
    }.

json_to_auth(JsonProps) ->
    #memw_auth{
        hash = check_hash(proplists:get_value(<<"hash">>, JsonProps)),

        %% The client already registered a session.
        session_id = proplists:get_value(<<"session_id">>, JsonProps)
    }.

json_to_event(JsonProps) ->
    #memw_event{
        type = proplists:get_value(<<"type">>, JsonProps),
        hash = check_hash(proplists:get_value(<<"hash">>, JsonProps)),
        data = proplists:get_value(<<"data">>, JsonProps)
    }.


%% Called when a message from javascript client was recieved
stream(Data, Req, State) ->
    lager:info("Message recieved ~p~n", [Data]),
    DecodedData = jsx:json_to_term(Data),
    lager:info("JSON: ~p~n", [DecodedData]),
    Rec = json_to_record(DecodedData),
    handle_event(Rec, Req, State).


handle_event(#memw_auth{} = A, Req, State) ->
    handle_auth(A, Req, State);

handle_event(A, Req, State=#state{hub=Hub}) ->
    memw_session:send(Hub, A),
    {ok, Req, State}.


%% First connect, no session
handle_auth(#memw_auth{session_id=undefined, hash=Hash} = A, 
    Req, State) ->
    {ok, Hub} = memw_session:start(self(), Hash),
    Ref = erlang:monitor(process, Hub),
    State2 = State#state{
        hub = Hub, hub_mon = Ref
    },
    {ok, Req, State2};

%% Session was already created. Check a session id.
handle_auth(#memw_auth{session_id=SessionId, hash=Hash} = A, 
    Req, State) ->
    {Success, State2} = 
    try
        Hub = memw_session:lookup_server(SessionId),
        ok = memw_session:update_connection(Hub, self()),
        Ref = erlang:monitor(process, Hub),
        {true, State#state{hub = Hub, hub_mon = Ref}}

    catch error:badarg ->
        {false, State}
    end,

    %% Send directly
    Cmd = fire_event(Hash, <<"authCheckResult">>, [{<<"success">>, Success}]),
    Respond = jsx:term_to_json(Cmd),
    {reply, Respond, Req, State2}.


%% Called when a process recieved a mess from other erlang process.
%% An other process calls ?MODULE:send/2, the bullet application recieves 
%% a message and calls ?MODULE:info/3.
info({command, Cmd}, Req, State) ->
    Respond = jsx:term_to_json(Cmd),
    {reply, Respond, Req, State}.


terminate(_Req, _State) ->
    ok.


%%
%% API
%%

send(Pid, Mess) ->
    Pid ! Mess.

send_command(Pid, Cmd) ->
    send(Pid, {command, Cmd}).


%%
%% Helpers
%%

fire_event(Hash, EventType) ->
    encode_action(<<"fireEvent">>, Hash, encode_event(EventType)).
    
fire_event(Hash, EventType, EventData) ->
    encode_action(<<"fireEvent">>, Hash, encode_event(EventType, EventData)).

add_listener(Hash, EventType) ->
    encode_action(<<"addListener">>, Hash, encode_event(EventType)).


encode_action(ActionType, Hash, EncodedEvent) ->
    [ {<<"type">>, ActionType}
    , {<<"hash">>, Hash}
    , {<<"data">>, EncodedEvent}].

encode_event(EventType) ->
    [ {<<"type">>, EventType}].

encode_event(EventType, EventData) ->
    [ {<<"type">>, EventType}
    , {<<"data">>, EventData}].


