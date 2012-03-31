-record(eqx_event, {
    id, 
    from, 
    manager,
    %% Event type (for example, set_value, get_value)
    type :: term(), 
    %% User data
    data :: term()
}).
