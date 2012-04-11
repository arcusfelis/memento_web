%% Feel free to use, reuse and abuse the code in this file.

-module(memw_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).
-define(APP, memento_web).

start() ->
    lager:start(),

    application:start(gproc),

    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(cowboy),

    application:start(nicedecimal),
    application:start(jsx),

    application:start(bullet),
    application:start(sendfile),
    application:start(cowboy_static),
    application:start(memento_web).


%% TODO: use envs for config
start(_Type, _Args) ->
    PrivDir = code:priv_dir(?APP),
    BuildDir = abs_path(filename:join(
            [PrivDir, "html"])),
    JQueryDir = abs_path(filename:join(
            [PrivDir, "jquery"])),
    BulletDir = abs_path(code:priv_dir(bullet)),

    RootRule = cowboy_static:rule([
        {dir, BuildDir}, 
        {prefix, ""}, 
        {sendfile, false}]),

    BulletRule = cowboy_static:rule([
        {dir, BulletDir}, 
        {prefix, "bullet"}, 
        {sendfile, false}]),

    JQueryRule = cowboy_static:rule([
        {dir, JQueryDir}, 
        {prefix, "jquery"}, 
        {sendfile, false}]),

    StreamRule = {[<<"stream">>], bullet_handler, 
                  [{handler, memw_stream_handler}]},
    DefaultRule = {[], memw_default_handler, []},
	%% These rules are active only in a source version!
	SourceRules = 
		[ cowboy_static:rule([
          		{dir, BuildDir}, 
          		{prefix, "priv/html"}, 
          		{sendfile, false}])
		, cowboy_static:rule([
          		{dir, code:lib_dir(?APP, q_deps)}, 
          		{prefix, "q_deps"}, 
          		{sendfile, false}])
		, cowboy_static:rule([
          		{dir, code:lib_dir(?APP, q_src)}, 
          		{prefix, "q_src"}, 
          		{sendfile, false}])],

    Dispatch = [
    	{'_', [StreamRule, DefaultRule, BulletRule, JQueryRule] 
			++ SourceRules
			++ [RootRule]}
    ],
    cowboy:start_listener(memw_http, 100,
    	cowboy_tcp_transport, [{port, 1080}],
    	cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
%   cowboy:start_listener(https, 100,
%       cowboy_ssl_transport, [
%       	{port, 1443}, {certfile, "priv/ssl/cert.pem"},
%       	{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
%       cowboy_http_protocol, [{dispatch, Dispatch}]
%   ),
    memw_sup:start_link().

stop(_State) ->
    ok.


%%
%% Private
%%

abs_path(Path) -> 
    filename:join(
        abs_path_(
            filename:split(
                filename:absname(Path)), [])).

abs_path_([".."|T], [_|Stack]) ->
    abs_path_(T, Stack);
abs_path_([H|T], Stack) ->
    abs_path_(T, [H|Stack]);
abs_path_([], Stack) ->
    lists:reverse(Stack).
