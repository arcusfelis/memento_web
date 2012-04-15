%% Income message types
-record(memw_action, {type, hash, path}).
-record(memw_event, {type, hash, data}).
-record(memw_auth, {hash, session_id}).

%% Example for all other objects
-record(memw_object, {hash, type}).

%% Real objects
-record(memw_table, {hash, type}).
-record(memw_form, {hash, type}).
