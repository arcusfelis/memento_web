-module(eqx_table_widget).
-behaviour(gen_event).

%% fields 		record_info()
%% visibility	record(true, false, undefined)
%% renderer		record(ClassName, undefined)
%% names		record(binary())
%% order        record(int())
-spec new(list()) -> pid().

new(Props) -> 
	[].


