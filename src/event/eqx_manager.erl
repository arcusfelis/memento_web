%% Event Manager
-module(eqx_manager).

-export([new/0, 
         send/2, 
         send/3,
         subscribe/1]).

-export([test/0]).

-include_lib("eqx/include/eqx.hrl").

%% Manager Module
-define(MM, ?MODULE). 


new() -> 
    {ok, Pid} = gen_event:start_link(),
    Pid.


subscribe(Srv) ->
    Args = [],
    subscribe(Srv, Args).


subscribe(Srv, Args) ->
    Handler = eqx_subscribe_handler,
    Ref = erlang:make_ref(),
    Args = [{from, self()}, {ref, Ref}] ++ Args,
    gen_event:add_sup_handler(Srv, Handler, Args),
    Ref.


send(Srv, Mess) -> 
    Event = #eqx_event{from=self(), manager=Srv, data=Mess},
    gen_event:notify(Srv, Event).


send(Srv, Type, Mess) -> 
    Event = #eqx_event{from=self(), manager=Srv, data=Mess, type=Type},
    gen_event:notify(Srv, Event).


test() ->
    S = ?MM:new(),
    erlang:spawn(fun() -> 
        ?MM:send(S, testMess), 
        ?MM:send(S, type, testMess) 
        end),
    ?MM:subscribe(S).

