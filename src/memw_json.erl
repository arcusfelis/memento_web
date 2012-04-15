-module(memw_json).
-include_lib("memento_core/include/records.hrl").

-export([record_to_json/1]).


agent_to_json(#m_agent{id=Id, name=Name, version=Version, address=Address, port=Port}) ->
    [ {id, Id}
    , {name, to_binary(Name)}
    , {address, address_to_binary(Address)}
    , {port, Port}
    , {version, atom_to_binary(Version)}
    ].
    

%% Check JSON object `{x:"y"}'
check_object(Object) ->
    [Field || {Key, Value} = Field <- Object, Value =/= undefined].


to_binary(undefined) -> 
    undefined;

to_binary(A) when is_atom(A) -> 
    atom_to_binary(A);

to_binary(L) -> 
    iolist_to_binary(L).


address_to_binary([A, B, C, D] = List) ->
    list_to_binary(io_lib:format("~B.~B.~B.~B", List));

address_to_binary(Other) ->
    to_binary(Other).


atom_to_binary(A) -> list_to_binary(atom_to_list(A)).


record_to_json(A=#m_agent{}) ->
    check_object(agent_to_json(A)).


test_agents() ->
    Data = 
    [ #m_agent{id=1, name= <<"Test Name 1">>, version= v2c, address= [127,0,0,1], port=666}
    , #m_agent{id=2, name= <<"Test Name 2">>, version= v2, address= [127,0,0,2], port=111}
    ],
    [ agent_to_json(Rec) || Rec <- Data ].
