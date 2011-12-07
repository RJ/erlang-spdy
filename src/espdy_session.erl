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

-export([start_link/3, snd/3]).

-compile(export_all). %% debug

-record(state, {
    spdy_opts,
    socket :: inet:socket(),
    transport :: module(),
    last_client_sid = 0,
    last_server_sid = 0,
    buffer = <<>> :: binary(),
    goingaway = false :: boolean(),
    z_headers, %% zlib context for headers
    settings = [] :: [{integer(), integer()}],
    streams = [] :: [{integer(), #stream{}}] %% use proplist for now
}).

%% API.

%% @doc Start a SPDY protocol process.
-spec start_link(inet:socket(), module(), any()) -> {ok, pid()}.
start_link(Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Socket, Transport, Opts], []).

snd(Pid, StreamID, Frame) when is_pid(Pid), is_integer(StreamID) ->
    gen_server:cast(Pid, {snd, StreamID, Frame}).

%%

init([Socket, Transport, Opts]) ->
    %% Init zlib context used for headers blocks
    Z = zlib:open(),
    ok = zlib:inflateInit(Z),
    State = #state{ socket=Socket, 
                    transport=Transport,
                    z_headers=Z,
                    spdy_opts=Opts
                  },
    ?LOG("SPDY_PROTO init ~p ~p",[self(), State]),
    {ok, State}.

handle_call(none_implemented, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({snd, _StreamID, Frame}, State) ->
    socket_write(Frame, State),
    {noreply, State}.

handle_info(shoot, State = #state{transport=Transport, socket=Socket}) ->
    ?LOG("Shoot.",[]),
    Transport:setopts(Socket, [{active,once}, binary, {packet,raw}]),
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
    Transport:setopts(Socket, [{active,once}]),
    Ret.

terminate(_Reason, State = #state{transport=Transport, socket=Socket, z_headers=Z}) ->
    catch Transport:close(Socket),
    catch zlib:close(Z),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% Write a packet to the socket, either raw binary, or auto-marshalling control/data frames
socket_write(F=#dframe{streamid=StreamID,
                       flags=Flags,
                       length=Length,
                       data=Data}, #state{transport=T,socket=S}) ->
    ?LOG("Sending frame: ~p",[F]),
    Packet = << 0:1, 
                StreamID:31/big-unsigned-integer,
                Flags:8/big-unsigned-integer, 
                Length:24/big-unsigned-integer,
                Data/binary
             >>,
    ?LOG("SEND (~p/~p): ~p",[Length, size(Data),Packet]),
    T:send(S, Packet);

socket_write(F=#cframe{type=Type, 
                     version=Version, 
                     flags=Flags, 
                     length=Length, 
                     data=Data}, #state{transport=T,socket=S}) ->
    ?LOG("Sending frame: ~p",[F]),
    Packet = << 1:1,
                Version:15/big-unsigned-integer, 
                Type:16/big-unsigned-integer, 
                Flags:8/big-unsigned-integer, 
                Length:24/big-unsigned-integer,
                Data/binary
            >>,
    ?LOG("SEND (~p/~p): ~p",[Length, size(Data),Packet]),
    T:send(S, Packet).

unpack(Z, Compressed, Dict) ->
    case catch zlib:inflate(Z, Compressed) of
         {'EXIT',{{need_dictionary,_DictID},_}} ->
                 zlib:inflateSetDictionary(Z, Dict),
                 iolist_to_binary(zlib:inflate(Z, []));
          Uncompressed ->
                 iolist_to_binary(Uncompressed)
     end.

 %% process buffer until no more full frames left
process_buffer(State = #state{buffer = Buffer}) ->
    case parse_frame(Buffer) of
        %% no full frame yet, read more data
        undefined -> 
            {noreply, State};
        %% control frame received
        {F = #cframe{}, Rest} ->
            NewState = handle_control_frame(F, State#state{buffer=Rest}),
            %% could be another frame in the buffer:
            process_buffer(NewState);
        %% data frame received
        {F = #dframe{}, Rest} ->
            NewState = handle_data_frame(F, State#state{buffer=Rest}),
            %% could be another frame in the buffer:
            process_buffer(NewState)
    end.

%% Match CONTROL frame
%% +----------------------------------+
%% |C| Version(15bits) | Type(16bits) |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+
parse_frame(<<  1:1, %% control bit
                Version:15/big-unsigned-integer, 
                Type:16/big-unsigned-integer, 
                Flags:8/big-unsigned-integer, 
                Length:24/big-unsigned-integer,
                Data:Length/binary-unit:8,
                Rest/binary
            >>) ->
    {#cframe{version=Version,
             type=Type,
             flags=Flags,
             length=Length,
             data=Data}, Rest};

%% Match DATA frame
%% +----------------------------------+
%% |C|       Stream-ID (31bits)       |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+
parse_frame(<<  0:1, %% control big clear = data
                StreamID:31/big-unsigned-integer,
                Flags:8/big-unsigned-integer, 
                Length:24/big-unsigned-integer,
                Data:Length/binary-unit:8,
                Rest/binary
             >>) ->
    {#dframe{streamid=StreamID,
             flags=Flags,
             length=Length,
             data=Data}, Rest};

%% No full frame found:
parse_frame(_Buffer) ->
    undefined. 

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


%% 2.6.9 Name/Value Header Block
parse_name_val_pairs(Bin, State) ->
    Unpacked = unpack(State#state.z_headers, Bin, ?HEADERS_ZLIB_DICT),
    zlib:inflateReset(State#state.z_headers),
    <<Num:16/big-unsigned-integer, Rest/binary>> = Unpacked,
    parse_name_val_pairs(Num, Rest, []).

parse_name_val_pairs(0, _, Acc) -> 
    lists:reverse(Acc);
parse_name_val_pairs(Num, << NameLen:16/big-unsigned-integer,
                    Name:NameLen/binary,
                    ValLen:16/big-unsigned-integer,
                    Val:ValLen/binary,
                    Rest/binary >>, Acc) ->
    %% TODO validate and throw errors as per 2.6.9
    Pair = {Name, Val},
    parse_name_val_pairs(Num-1, Rest, [Pair | Acc]).
%% END nvpair stuff



merge_headers(NewList, OldList) ->
    NewList ++ OldList. %% TODO (overwrite, no dupes, etc)  

%% syntactic sugar
hasflag(Flags, Flag) -> (Flags band Flag) == Flag.


%% CONTROL FRAMES:

%% The SYN_STREAM control frame allows the sender to asynchronously create a stream between the endpoints.
handle_control_frame(#cframe{   type = ?SYN_STREAM,
                                version = 2,
                                flags=Flags,
                                data = << _:1, StreamID:31/big-unsigned-integer,
                                          _:1, AssocStreamID:31/big-unsigned-integer,
                                          Priority:2/big-unsigned-integer, %% size is 3 in v3
                                          _Unused:14/binary-unit:1, %% size is 12 in v3
                                          NVPairsData/binary
                                       >>
                                }, State=#state{}) ->
    %% If the client is initiating the stream, the Stream-ID must be odd. 0 is not a valid Stream-ID.
    %% Stream-IDs from each side of the connection must increase monotonically as new streams are created.
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
            Headers = parse_name_val_pairs(NVPairsData, State),
           {ok, Pid} = espdy_stream:start_link(StreamID, 
                                                     self(), 
                                                     Headers, 
                                                     espdy_stream_http, 
                                                     State#state.z_headers,
                                                     State#state.spdy_opts),
            S = #stream{id=StreamID,
                        pid=Pid,
                        associd=AssocStreamID,
                        priority=Priority,
                        headers=Headers,
                        clientclosed=hasflag(Flags, ?CONTROL_FLAG_UNIDIRECTIONAL),
                        fin=hasflag(Flags, ?CONTROL_FLAG_FIN),
                        syn_replied=true %% since we are about to send one
                       },
            ?LOG("NEW STREAM: ~p",[S]),
            NewState = State#state{ streams = [ {StreamID, S} | State#state.streams ] },
            NewState
    end;

handle_control_frame(#cframe{   type = ?SYN_REPLY, %% stream creation initiated by us was accepted
                                flags = Flags,
                                data = << _:1, StreamID:31/big-unsigned-integer,
                                          _NVPairsData/binary
                                       >>
                                }, State=#state{}) ->
    case lookup_stream(StreamID, State) of
        undefined -> 
            session_error(protocol_error, State), %% TODO what sort of error?
            State;
        S = #stream{syn_replied=true} ->
            %% If an endpoint receives multiple SYN_REPLY frames for the same active stream ID, 
            %% it MUST issue a stream error (Section 2.4.2) with the error code STREAM_IN_USE.
            stream_error(stream_in_use, S, State),
            State;
        S = #stream{} ->
            %% TODO a stream we initiated is now set up and ready to rock.. tell someone?
            NewS = S#stream{syn_replied=true, clientclosed=hasflag(Flags,?CONTROL_FLAG_FIN)},
            NewState = update_stream(StreamID, NewS, State),
            NewState
    end;

handle_control_frame(#cframe{   type = ?RST_STREAM, %% RST_STREAM
                                flags = 0,
                                length = 8,
                                data = << _:1, StreamID:31/big-unsigned-integer,
                                          StatusCode:32/big-unsigned-integer
                                       >>
                                }, State=#state{}) ->
    Status = status_code_to_atom(StatusCode),
    io:format("RST_STREAM ~p status: ~p",[StreamID, Status]),
    case lookup_stream(StreamID, State) of
        undefined ->
            session_error(protocol_error, State),
            State;
        #stream{pid=Pid} ->
            espdy_stream:closed(Pid, Status),
            NewState = remove_stream(StreamID, State),
            NewState
    end;

handle_control_frame(#cframe{   type = ?SETTINGS, %% SETTINGS
                                flags=_Flags,
                                data = << SettingsData >>
                              }, State=#state{}) ->
    Settings = parse_settings(SettingsData),
    NewState = apply_settings(Settings, State),
    NewState;

handle_control_frame(#cframe{   type=?NOOP,
                                version=2,
                                length=0}, State=#state{}) ->
    State;

handle_control_frame(F=#cframe{ type = ?PING,
                                length = 4,
                                data = << _PingID:32/big-unsigned-integer >>
                              }, State=#state{}) ->
    ?LOG("GOT PING",[]),
    socket_write(F, State), 
    State;

handle_control_frame(#cframe{   type = ?GOAWAY,
                                length = 4,
                                data = << _:1, _LastGoodStreamID:31/big-unsigned-integer >> %% in v3: , StatusCode:32/big-unsigned-integer >>
                              }, State=#state{}) ->
    lists:foreach(fun(#stream{pid=Pid}) ->
                espdy_stream:closed(Pid, goaway)
    end, State#state.streams),
    exit(normal), %% TODO shouldn't exit here, need to flush buffers?
    State#state{goingaway=true};

handle_control_frame(#cframe{   type = ?HEADERS,
                                data = << _:1, StreamID:31/big-unsigned-integer,
                                          _Unused:16/binary, %% not in v3?
                                          NVPairsData/binary
                                       >>
                                }, State=#state{}) ->
    case lookup_stream(StreamID, State) of
        undefined ->
            session_error(protocol_error, State), %% TODO which error?
            State;
        S=#stream{headers=H} ->
            Headers = parse_name_val_pairs(NVPairsData, State),
            H2 = merge_headers(Headers, H),
            NewStream = S#stream{headers=H2},
            NewState = update_stream(StreamID, NewStream, State),
            espdy_stream:headers_updated(S#stream.pid, Headers, H2),
            NewState
    end;

%% TODO lots of flow control stuff to do (read 2.6.8 again..) v3 ONLY!
handle_control_frame(#cframe{   type = 9, %% WINDOW_UPDATE
                                length=8,
                                data = << _:1, StreamID:31/big-unsigned-integer,
                                          _:1, DeltaWindowSize:31/big-unsigned-integer
                                       >>
                                }, State=#state{}) when DeltaWindowSize >= 1, DeltaWindowSize =< 16#7fffffff ->
    case lookup_stream(StreamID, State) of
        undefined ->
            session_error(protocol_error, State), %% TODO which error?
            State;
        S=#stream{window=W} ->
            NewStream = S#stream{window=W+DeltaWindowSize},
            NewState = update_stream(StreamID, NewStream, State),
            NewState
    end;

    %% If an endpoint receives a control frame for a type it does not recognize, it MUST ignore the frame:
handle_control_frame(F = #cframe{}, State=#state{}) ->
    ?LOG("UNHANDLED CONTROL FRAME: ~p",[F]),
    State.

%% DATA FRAME:
handle_data_frame(#dframe{  streamid=StreamID,
                            flags=Flags,
                            data=Data
                         }, State=#state{}) ->
    case lookup_stream(StreamID, State) of
        undefined ->
            StatusCode = ?INVALID_STREAM,
            F = #cframe{type=?RST_STREAM,
                        length=8,
                        data = <<0:1, StreamID:31/big-unsigned-integer, StatusCode:32/big-unsigned-integer>>},
            socket_write(F, State),
            State;
        S = #stream{} -> %% TODO check stream is known to be active still?
            espdy_stream:received_data(S#stream.pid, Data),
            case hasflag(Flags,?DATA_FLAG_FIN) of
                false ->
                    State;
                true ->
                    NewS = S#stream{fin=true},
                    NewState = update_stream(StreamID, NewS, State),
                    NewState
            end
    end.

session_error(Err, State = #state{}) ->
    StatusCode = atom_to_status_code(Err),
    LastGoodStreamID = State#state.last_client_sid,
    F = #cframe{type=7, %% GOAWAY
                length=8,
                data = << 0:1, LastGoodStreamID:31/big-unsigned-integer,
                          StatusCode:32/big-unsigned-integer >>},
    socket_write(F, State),
    exit(normal).

stream_error(Err, #stream{id=Id}, State = #state{}) ->
    StatusCode = atom_to_status_code(Err),
    F = #cframe{type=3, %% RST_STREAM
                length=8,
                data = <<0:1, Id:31/big-unsigned-integer, StatusCode:32/big-unsigned-integer>>},
    socket_write(F, State),
    %% close/delete stream:
    NewState = remove_stream(Id, State),
    NewState.

parse_settings(<<Num:32/big-unsigned-integer, Data/binary>>) ->
    take_settingpair(Num, Data, []).

take_settingpair(0, _, Acc) -> lists:reverse(Acc);
take_settingpair(Num, <<Flags:8/big-unsigned-integer,
                        Id:24/big-unsigned-integer,
                        Value:32/big-unsigned-integer,
                        Rest/binary>>, Acc) ->
    Item = {Id, Flags, Value},
    take_settingpair(Num-1, Rest, [Item|Acc]).

apply_settings(Settings, State = #state{settings=OldSettings}) ->
    %% TODO persist settings somehow, if flags has perist bit set for a value
    NewSettings = lists:foldl(fun({Id, Flags, Value}, Acc) ->
        [ {Id, {Flags, Value}} | proplists:delete(Id, Acc) ]
    end, OldSettings, Settings),
    State#state{settings=NewSettings}.
%% STATUS CODES used by rst-stream, goaway, etc

atom_to_status_code(protocol_error)         -> 1;
atom_to_status_code(invalid_stream)         -> 2;
atom_to_status_code(refused_stream)         -> 3;
atom_to_status_code(unsupported_version)    -> 4;
atom_to_status_code(cancel)                 -> 5;
atom_to_status_code(flow_control_error)     -> 6;
atom_to_status_code(stream_in_use)          -> 7;
atom_to_status_code(stream_already_closed ) -> 8.

status_code_to_atom(1)  -> protocol_error;
status_code_to_atom(2)  -> invalid_stream;
status_code_to_atom(3)  -> refused_stream;
status_code_to_atom(4)  -> unsupported_version;
status_code_to_atom(5)  -> cancel;
status_code_to_atom(6)  -> flow_control_error;
status_code_to_atom(7)  -> stream_in_use;
status_code_to_atom(8)  -> stream_already_closed.

