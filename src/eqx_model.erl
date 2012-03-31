-module(eqx_model).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).



%% fields 		record_info()
%% visibility	record(true, false, undefined)
%% renderer		record(ClassName, undefined)
%% names		record(binary())
%% order        record(int())
%% validator    module:fun(record())
-spec new(list()) -> pid().

new(Props) -> 
	[].



get(Srv, Key) ->
    gen_server:call(Srv, {get, Key}).


set(Srv, Key, Value) ->
    gen_server:call(Srv, {set, Key, Value}).


state(Srv) ->
    gen_server:call(Srv, state).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

