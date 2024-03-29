-module(memw_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    SessionSup = ?CHILD(memw_session_sup, worker),
	{ok, {{one_for_one, 10, 10}, [SessionSup]}}.
