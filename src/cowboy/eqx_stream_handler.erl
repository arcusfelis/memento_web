-module(eqx_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).
-export([send/2]).

-record(state, {
}).


init(_Transport, Req, _Opts, _Active) ->
    State = #state{
    },

    {ok, Req, State}.


%% Called when a message from javascript client was recieved
stream(Data, Req, State) ->
    lager:info("Message recieved ~p~n", [Data]),
    {reply, <<>>, Req, State}.

%stream(<<"all_torrents">> = _Data, Req, State) ->
%    Data = ?HUB:all_torrents(),
%    Respond = [{'event', <<"dataLoadCompleted">>} 
%              ,{'data', [{'rows', Data}]}
%              ],
%    EncodedRespond = jsx:term_to_json(Respond),
%    {reply, EncodedRespond, Req, State};
%
%stream(<<"all_peers">> = _Data, Req, State) ->
%    Data = eqx_peers:all_peers(),
%    Respond = [{'event', <<"peerDataLoadCompleted">>} 
%              ,{'data', [{'rows', Data}]}
%              ],
%    EncodedRespond = jsx:term_to_json(Respond),
%    {reply, EncodedRespond, Req, State};
%
%stream(Data, Req, State) ->
%    DecodedData = jsx:json_to_term(Data),
%    case proplists:get_value(<<"event">>, DecodedData) of
%    <<"remove">> -> 
%        Id = proplists:get_value(<<"id">>, DecodedData),
%        true = is_number(Id),
%        {reply, Data, Req, State};
%
%    <<"pause">> -> 
%        Ids = proplists:get_value(<<"ids">>, DecodedData),
%        lists:map(fun etorrent_ctl:pause/1, Ids),
%        {reply, Data, Req, State};
%
%    <<"continue">> -> 
%        Ids = proplists:get_value(<<"ids">>, DecodedData),
%        lists:map(fun etorrent_ctl:continue/1, Ids),
%        {reply, Data, Req, State};
%
%    <<"file_list">> ->
%        %% Load file list
%        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
%        Parents   = proplists:get_value(<<"parent_ids">>, DecodedData),
%
%        Nodes = lists:map(fun(ParentId) ->
%                Children = etorrent_io:tree_children(TorrentId, ParentId),
%                [ {'parent_id', ParentId}
%                , {'children', Children}
%                ]
%            end, Parents),
%        
%        Respond = [ {'event', <<"fileDataLoadCompleted">>} 
%                  , {'data', [ {'torrent_id', TorrentId}
%                             , {'nodes', Nodes}]}
%                  ],
%        EncodedRespond = jsx:term_to_json(Respond),
%        {reply, EncodedRespond, Req, State};
%
%    <<"wish_list">> ->
%        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
%        {ok, Wishes} = etorrent_torrent_ctl:get_wishes(TorrentId),
%
%        EncodedRespond = encode_wishes(TorrentId, Wishes),
%        {reply, EncodedRespond, Req, State};
%
%    <<"replace_wish_list">> ->
%        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
%        List = proplists:get_value(<<"list">>, DecodedData),
%        Wishes = [[{ binary_to_existing_atom(K)
%                   , if
%                        is_binary(V) -> binary_to_existing_atom(V);
%                        true -> V
%                     end} 
%                    || {K, V} <- X, K =/= <<"name">>] || X <- List],
%        error_logger:info_msg("Set new wishes ~p for torrent ~B.",
%            [Wishes, TorrentId]),
%        {ok, Wishes1} = etorrent_torrent_ctl:set_wishes(TorrentId, Wishes),
%        {ok, Req, State};
%
%    <<"wish_files">> ->
%        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
%        Fids      = proplists:get_value(<<"file_ids">>, DecodedData),
%        {ok, NewWishes} = etorrent_torrent_ctl:wish_file(TorrentId, Fids),
%        
%        EncodedRespond = encode_wishes(TorrentId, NewWishes),
%        {reply, EncodedRespond, Req, State};
%
%    <<"file_info">> ->
%        %% Load tracks list
%        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
%        FileId    = proplists:get_value(<<"file_id">>, DecodedData),
%        {ok, Req, State};
%
%    _ -> 
%        {reply, Data, Req, State}
%    end.




%% Called when a process recieved a mess from other erlang process.
%% An other process calls ?MODULE:send/2, the bullet application recieves 
%% a message and calls ?MODULE:info/3.
info(_Info, Req, State) ->
    {reply, <<>>, Req, State}.
	
%info({'diff_list', Rows}=_Info, Req, State) ->
%    Respond = [{'event', <<"dataUpdated">>} 
%              ,{'data', [{'rows', Rows}]}
%              ],
%    EncodedRespond = jsx:term_to_json(Respond),
%    {reply, EncodedRespond, Req, State};
%
%info({'add_list', Rows}=_Info, Req, State) ->
%    Respond = [{'event', <<"dataAdded">>} 
%              ,{'data', [{'rows', Rows}]}
%              ],
%    EncodedRespond = jsx:term_to_json(Respond),
%    {reply, EncodedRespond, Req, State};
%
%info({'delete_list', Rows}=_Info, Req, State) ->
%    Respond = [{'event', <<"dataRemoved">>} 
%              ,{'data', [{'rows', Rows}]}
%              ],
%    EncodedRespond = jsx:term_to_json(Respond),
%    {reply, EncodedRespond, Req, State};
%
%info({'log_event', Mess}=_Info, Req, State) ->
%    Respond = [{'event', <<"logEvent">>} 
%              ,{'data', Mess}
%              ],
%    EncodedRespond = jsx:term_to_json(Respond),
%    {reply, EncodedRespond, Req, State};
%
%info({'track_list', TorrentId, FileId, Props}=_Info, Req, State) ->
%    Respond = [ {'event', <<"tracksDataLoadCompleted">>} 
%              , {'data', [ {'torrent_id', TorrentId}
%                         , {'file_id', FileId}
%                         , {'rows', Props}]}
%              ],
%    EncodedRespond = jsx:term_to_json(Respond),
%    {reply, EncodedRespond, Req, State}.


terminate(_Req, _State) ->
    ok.


%%
%% API
%%

send(Pid, Mess) ->
    Pid ! Mess.
