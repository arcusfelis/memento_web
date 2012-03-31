-module(eqx_subscribe_handler).

%% otp behaivior callbacks
-export([init/1, 
         handle_event/2, 
         handle_info/2, 
         terminate/2, 
         handle_call/2, 
         code_change/3]).

%% internal export
-export([default_handler/3]).

-record(state, {subscriber, ref, handler}).
-include_lib("eqx/include/eqx.hrl").


%% ==================================================================

%% @private
init(Args) ->
    From = proplists:get_value(from, Args),
    true = erlang:is_process_alive(From),
    S = #state{
            subscriber=From,
            handler=proplists:get_value(handler, Args, 
                fun ?MODULE:default_handler/3),
            ref=proplists:get_value(ref, Args)
        },
    {ok, S}.

%% @private
handle_event(#eqx_event{from=Sub}, S=#state{subscriber=Sub}) ->
    {ok, S};

handle_event(Event=#eqx_event{}, 
    S=#state{subscriber=Sub, ref=Ref, handler=Fn}) ->
    Fn(Sub, Ref, Event),
    {ok, S}.

%% @private
handle_info(_, State) ->
    {ok, State}.

%% @private
terminate(_, _State) ->
    ok.

%% @private
handle_call(null, State) ->
    {ok, null, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ==================================================================

%% @doc It is a function M:F/3 to run when an event Event transmited 
%%      to an subscriber.
%%      The default subscriber do not check an event type.
default_handler(Sub, Ref, Event) ->
    Sub ! {eqx_event, Ref, Event}.
