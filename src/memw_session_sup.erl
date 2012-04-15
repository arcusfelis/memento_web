-module(memw_session_sup).
-export([start_link/0, init/1]).

-define(SUPERVISOR, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
  SessionSpec = {memw_session, {memw_session, start_link, []},
        temporary, 2000, worker, [memw_session]},
  StartSpecs = {{simple_one_for_one, 0, 1}, [SessionSpec]},
  {ok, StartSpecs}.
