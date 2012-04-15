-module(memw_meta).
-export([ type_to_table_name/1
        , type_to_record_name/1]).

type_to_record_name(Type) -> 
    type_to_table_name(Type).

type_to_table_name(<<"manager">>) -> m_manager;
type_to_table_name(<<"trap">>)    -> m_trap;
type_to_table_name(<<"object">>)  -> m_object;
type_to_table_name(<<"agent">>)   -> m_agent;
type_to_table_name(<<"user">>)    -> m_user.
