-module(memw_json).
-include_lib("memento_core/include/records.hrl").


agent_to_json(#m_agent{id=Id, name=Name, version=Version, address=Address, port=Port}) ->
    [ {id, Id}
    , {name, iolist_to_binary(Name)}
    , {address, address_to_binary(Address)}
    , {port, Port}
    , {version, atom_to_binary(Version)}
    ].


address_to_binary([A, B, C, D] = List) ->
    list_to_binary(io_lib:format("~B.~B.~B.~B", List)).

atom_to_binary(A) -> list_to_binary(atom_to_list(A)).
