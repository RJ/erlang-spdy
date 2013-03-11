%% SPDY
%% Current version: http://www.chromium.org/spdy/spdy-protocol/spdy-protocol-draft2
%%
%% (as-yet) unimplemented in chrome draft v3:
%% http://mbelshe.github.com/SPDY-Specification/draft-mbelshe-spdy-00.xml
%%
%% This is in no way finished, or even working properly.
%%
%% Start a new chrome like so:
%%   chromium-browser --use-spdy=no-ssl --new-window --user-data-dir=/tmp/foo 
%% and visit http://localhost:6121/
%%
-module(espdy_session).
-behaviour(gen_server).
-include("include/espdy.hrl").
-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/4, snd/3]).

-compile(export_all). %% debug

-record(state, {
    spdy_opts,
    spdy_version = 3 :: integer(),
    socket :: inet:socket(),
    transport :: module(),
    last_client_sid = 0,
    last_server_sid = 0,
    cbmod, %% callback module for streams
    buffer = <<>> :: binary(),
    goingaway = false :: boolean(),
    z_context_inf,  %% zlib context for inflating
    z_context_def,  %% zlib context for deflating
    settings = [] :: [{integer(), integer()}],
    streams = [] :: [{integer(), #stream{}}] %% use proplist for now
}).

%% API.

-spec start_link(inet:socket(), gen_tcp | ssl, module(), list()) -> {ok, pid()}.
start_link(Socket, Transport, CBMod, Opts) ->
    gen_server:start(?MODULE, [Socket, Transport, CBMod, Opts], []).

snd(Pid, StreamID, Frame) when is_pid(Pid), is_integer(StreamID) ->
    gen_server:cast(Pid, {snd, StreamID, Frame}).

%%

init([Socket, Transport, CBMod, Opts]) ->
    SpdyVersion = proplists:get_value(spdy_version, Opts, 3),
    %% Init zlib context used for headers blocks
    Zinf = zlib:open(),
    ok = zlib:inflateInit(Zinf),
    Zdef = zlib:open(),
    ok = zlib:deflateInit(Zdef),
    %%ok = zlib:deflateInit(Z, best_compression,deflated, 15, 9, default),
    case SpdyVersion of
        2 -> zlib:deflateSetDictionary(Zdef, ?HEADERS_ZLIB_DICT);
        3 -> zlib:deflateSetDictionary(Zdef, ?HEADERS_ZLIB_DICT_V3)
    end,
    State = #state{ socket=Socket,
                    cbmod=CBMod,
                    transport=Transport,
                    z_context_inf=Zinf,
                    z_context_def=Zdef,
                    spdy_version = SpdyVersion,
                    spdy_opts=Opts
                  },
    ?LOG("SPDY_VERSION init v~B ~p ~p",[SpdyVersion, self(), State]),
    {ok, State}.

handle_call(none_implemented, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({snd, _StreamID, Frame}, State) ->
    socket_write(Frame, State),
    {noreply, State}.

%% means we are the controlling process for the socket: go go go
handle_info(shoot, State = #state{transport=Transport, socket=Socket}) ->
    ?LOG("Shoot.",[]),
    case Transport of 
        gen_tcp -> inet:setopts(Socket, [{active,once}, binary, {packet,raw}]);
        ssl     -> ssl:setopts(Socket, [{active,once}, binary, {packet,raw}])
    end,
    {noreply, State};

handle_info({ssl_closed, Sock}, State) ->
    handle_info({tcp_closed, Sock}, State);
handle_info({tcp_closed, Socket}, State = #state{socket=Socket}) ->
    ?LOG("TCP_CLOSED",[]),
    {stop, normal, State};

handle_info({ssl, Sock, Data}, State) ->
    handle_info({tcp, Sock, Data}, State);
handle_info({tcp, Socket, Data}, State = #state{socket=Socket, transport=Transport, buffer=Buffer}) ->
    ?LOG("INCDATA: ~p\n", [Data]),
    Ret = process_buffer(State#state{buffer = <<Buffer/binary, Data/binary>>}),
    case Transport of
        gen_tcp -> inet:setopts(Socket, [{active,once}]);
        ssl     -> ssl:setopts(Socket, [{active,once}])
    end,
    Ret.

terminate(_Reason, _State = #state{transport=Transport, socket=Socket}) ->
    catch Transport:close(Socket),
%%    catch zlib:close(Z),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Write a packet to the socket, either raw binary, or auto-marshalling control/data frames
socket_write(F, #state{transport=T,socket=S, z_context_def=Z}) ->
    ?LOG("Sending frame: ~w",[F]),
    Packet = espdy_parser:build_frame(F, Z),
    T:send(S, Packet).


 %% process buffer until no more full frames left
process_buffer(State = #state{buffer = Buffer, z_context_inf = Z}) ->
    case espdy_parser:parse_frame(Buffer, Z) of
        %% no full frame yet, read more data
        undefined ->
            {noreply, State};
        {undefined, Rest} ->
            ?LOG("Dropped unhandled control frame",[]),
            process_buffer(State#state{buffer=Rest});
        %% frame received
        {F, Rest} when is_tuple(F) -> %% will be a #spdy_ record
            ?LOG("GOT FRAME: ~w",[F]),
            NewState = handle_frame(F, State#state{buffer=Rest}),
            %% could be another frame in the buffer:
            process_buffer(NewState)
    end.

%% MANAGEMENT OF ACTIVE STREAMS
%% stored in a proplist for now.

%% returns undefined | #stream{}
lookup_stream(Id, #state{streams=Slist}) when is_integer(Id), Id > 0 ->
    proplists:get_value(Id, Slist).

update_stream(Id, NewStream=#stream{}, State=#state{streams=Slist}) when is_integer(Id), Id > 0 ->
    NewSlist = [ {Id, NewStream} | proplists:delete(Id, Slist) ],
    State#state{streams=NewSlist}.

remove_stream(Id, State=#state{streams=Slist}) when is_integer(Id), Id > 0 ->
    NewSlist = proplists:delete(Id, Slist),
    State#state{streams=NewSlist}.
%% END MANAGEMENT OF ACTIVE STREAMS




merge_headers(NewList, OldList) ->
    NewList ++ OldList. %% TODO (overwrite, no dupes, etc)

%% syntactic sugar
hasflag(Flags, Flag) -> (Flags band Flag) == Flag.


%% CONTROL FRAMES:

%% The SYN_STREAM control frame allows the sender to asynchronously create a stream between the endpoints.
handle_frame(#spdy_syn_stream{  flags=Flags,
                                streamid=StreamID,
                                associd=AssocStreamID,
                                priority=Priority,
                                headers=Headers
                             }, State=#state{}) ->
    %% If the client is initiating the stream, the Stream-ID must be odd.
    %% 0 is not a valid Stream-ID. Stream-IDs from each side of the connection
    %% must increase monotonically as new streams are created.
    case StreamID =:= 0 orelse
         StreamID rem 2 =:= 0 orelse
         StreamID =< State#state.last_client_sid of
        true -> session_error(protocol_error, State);
        false -> ok
    end,
    %% It is a protocol error to send two SYN_STREAMS with the same stream-id.
    %% If a recipient receives a second SYN_STREAM for the same stream, it MUST issue a stream error with the status code PROTOCOL_ERROR.
    case lookup_stream(StreamID, State) of
        S = #stream{} ->
            stream_error(protocol_error, S, State),
            State;
        undefined ->
           {ok, Pid} = espdy_stream:start_link(StreamID,
                                               self(),
                                               Headers,
                                               State#state.cbmod,
                                               State#state.spdy_opts),
            %% TODO pass fin into startlink?
            hasflag(Flags,?DATA_FLAG_FIN) andalso espdy_stream:received_fin(Pid),
            S = #stream{id=StreamID,
                        pid=Pid,
                        associd=AssocStreamID,
                        priority=Priority,
                        headers=Headers,
                        syn_replied=true %% since we are about to send one
                       },
            ?LOG("NEW STREAM: ~p",[S]),
            NewState = State#state{ streams = [ {StreamID, S} | State#state.streams ],
                                    last_client_sid = StreamID 
                                  },
            NewState
    end;
handle_frame(#spdy_syn_stream{version=FrameVersion,
                              streamid=StreamID},
             State=#state{spdy_version=SessionVersion}) ->
    ?LOG("STREAM VERSION: ~p, SESSION VERSION: ~p", [FrameVersion, SessionVersion]),
    stream_error(unsupported_version, #stream{id=StreamID}, State),
    State;

handle_frame(#spdy_syn_reply{ flags = Flags,
                                      streamid = StreamID,
                                      headers=_H }, State=#state{}) ->
    case lookup_stream(StreamID, State) of
        undefined ->
            session_error(protocol_error, State), %% TODO what sort of error?
            State;
        S = #stream{syn_replied=true} ->
            %% If an endpoint receives multiple SYN_REPLY frames for the same
            %% active stream ID, it MUST issue a stream error (Section 2.4.2)
            %% with the error code STREAM_IN_USE.
            stream_error(stream_in_use, S, State),
            State;
        S = #stream{} ->
            %% TODO a stream we initiated is now set up and ready to rock.. tell someone?
            NewS = S#stream{syn_replied=true},
            hasflag(Flags,?DATA_FLAG_FIN) andalso espdy_stream:received_fin(S#stream.pid),
            NewState = update_stream(StreamID, NewS, State),
            NewState
    end;

handle_frame(#spdy_rst_stream{ streamid=StreamID,
                                       statuscode=StatusCode }, State=#state{}) ->
    Status = espdy_parser:status_code_to_atom(StatusCode),
    ?LOG("RST_STREAM ~p status: ~p",[StreamID, Status]),
    case lookup_stream(StreamID, State) of
        undefined ->
            session_error(protocol_error, State),
            State;
        #stream{pid=Pid} ->
            espdy_stream:closed(Pid, Status),
            NewState = remove_stream(StreamID, State),
            NewState
    end;

handle_frame(#spdy_settings{ flags=_Flags,
                             settings=Settings }, State=#state{}) ->
    NewState = apply_settings( Settings, State),
    NewState;

handle_frame(#spdy_noop{}, State) ->
    State;

handle_frame(F=#spdy_ping{}, State=#state{}) ->
    socket_write(F, State),
    State;

handle_frame(#spdy_goaway{lastgoodid=_LastGoodStreamID}, State=#state{}) ->
    lists:foreach(fun(#stream{pid=Pid}) ->
                espdy_stream:closed(Pid, goaway)
    end, State#state.streams),
    exit(normal), %% TODO shouldn't exit here, need to flush buffers?
    State#state{goingaway=true};

handle_frame(#spdy_headers{ streamid=StreamID,
                            flags=Flags,
                            headers=Headers }, State=#state{}) ->
    case lookup_stream(StreamID, State) of
        undefined ->
            session_error(protocol_error, State), %% TODO which error?
            State;
        S=#stream{headers=H} ->
            H2 = merge_headers(Headers, H),
            NewStream = S#stream{headers=H2},
            NewState = update_stream(StreamID, NewStream, State),
            espdy_stream:headers_updated(S#stream.pid, Headers, H2),
            hasflag(Flags,?DATA_FLAG_FIN) andalso espdy_stream:received_fin(S#stream.pid),
            NewState
    end;

%% DATA FRAME:
handle_frame(#spdy_data{ streamid=StreamID,
                         flags=Flags,
                         data=Data }, State=#state{}) ->
    case lookup_stream(StreamID, State) of
        undefined ->
            F = #spdy_rst_stream{version=State#state.spdy_version,
                                 streamid=StreamID,
                                 statuscode=?INVALID_STREAM},
            socket_write(F, State),
            State;
        S = #stream{} -> %% TODO check stream is known to be active still?
            espdy_stream:received_data(S#stream.pid, Data),
            hasflag(Flags,?DATA_FLAG_FIN) andalso espdy_stream:received_fin(S#stream.pid),
            State
    end.

session_error(_Err, State = #state{}) ->
    LastGoodStreamID = State#state.last_client_sid,
    % need to figure out StatusCode for v3
    F = #spdy_goaway{version=State#state.spdy_version,
                     lastgoodid=LastGoodStreamID},
    socket_write(F, State),
    exit(normal).

stream_error(Err, #stream{id=Id}, State = #state{}) ->
    StatusCode = espdy_parser:atom_to_status_code(Err),
    F = #spdy_rst_stream{version=State#state.spdy_version,
                         streamid=Id,
                         statuscode=StatusCode},
    socket_write(F, State),
    %% close/delete stream:
    NewState = remove_stream(Id, State),
    NewState.


apply_settings(Settings, State = #state{settings=OldSettings}) ->
    NewSettings = lists:foldl(fun(#spdy_setting_pair{id=Id, flags=Flags, value=Value}, Acc) ->
        [ #spdy_setting_pair{id=Id, flags=Flags, value=Value} | proplists:delete(Id, Acc) ]
    end, OldSettings, Settings),
    ?LOG("SETTINGS FOR THIS SESSION: ~p",[NewSettings]),
    State#state{settings=NewSettings}.
%% STATUS CODES used by rst-stream, goaway, etc


