-module(memw_json).
-include_lib("memento_core/include/records.hrl").

-export([record_to_json/1]).
-export([json_to_record/2]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

record_to_json(A=#m_agent{}) ->
    check_object(agent_to_json(A)).

json_to_record(m_agent, Data) ->
    agent_to_record(Data).


%% ------------------------------------------------------------------
%% Encoder
%% ------------------------------------------------------------------


agent_to_json(#m_agent{id=Id, name=Name, version=Version, address=Address, port=Port}) ->
    [ {id, Id}
    , {name, to_binary(Name)}
    , {address, address_to_binary(Address)}
    , {port, Port}
    , {version, to_binary(Version)}
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


address_to_binary([_A, _B, _C, _D] = List) ->
    list_to_binary(io_lib:format("~B.~B.~B.~B", List));

address_to_binary(Other) ->
    to_binary(Other).


atom_to_binary(A) -> list_to_binary(atom_to_list(A)).


%% ------------------------------------------------------------------
%% Decoder
%% ------------------------------------------------------------------

agent_to_record(Data) ->
    #m_agent{
        id = proplists:get_value(<<"id">>, Data),
        name = proplists:get_value(<<"name">>, Data),
        address = address_to_list(proplists:get_value(<<"address">>, Data)),
        port = proplists:get_value(<<"port">>, Data),
        version = fix_version(proplists:get_value(<<"version">>, Data))
    }.

address_to_list(Encoded) ->
    Decoded = io_lib:fread("~d.~d.~d.~d", to_list(Encoded)),
    case Decoded of
    {ok,ABCD,[]} ->
        ABCD;
    _Other ->
        Encoded
    end.


%% Not for Unicode
to_list(Bin) when is_binary(Bin) -> 
    binary_to_list(Bin);

to_list(Other) -> 
    Other.


fix_version(<<"v1">>)   -> v1;
fix_version(<<"v2">>)   -> v2;
fix_version(<<"v2c">>)  -> v2c;
fix_version(<<"v3">>)   -> v3.


%% ------------------------------------------------------------------
%% Tests 
%% ------------------------------------------------------------------


test_agents() ->
    Data = 
    [ #m_agent{id=1, name= <<"Test Name 1">>, version= v2c, address= [127,0,0,1], port=666}
    , #m_agent{id=2, name= <<"Test Name 2">>, version= v2, address= [127,0,0,2], port=111}
    ],
    [ agent_to_json(Rec) || Rec <- Data ].
