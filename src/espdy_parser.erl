%%
%% Parses frames from wire format to #spdy_* records (parse_frame)
%% and rebuilds wire format from #spdy_* records (build_frame)
%%
%% Doesn't do any inflating of name/val blocks.
%%
-module(espdy_parser).
-include("include/espdy.hrl").
-compile(export_all).

%% Match DATA frame
%% +----------------------------------+
%% |C|       Stream-ID (31bits)       |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+
parse_frame(<<  0:1, %% control bit clear = data
                StreamID:31/big-unsigned-integer,
                Flags:8/big-unsigned-integer, 
                Length:24/big-unsigned-integer,
                Data:Length/binary-unit:8,
                Rest/binary
             >>) ->
    {#spdy_data{streamid=StreamID,
                flags=Flags,
                data=Data}, Rest};

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
    {parse_control_frame(Version, Type, Flags, Length, Data), Rest};


%% No full frame found:
parse_frame(_Buffer) ->
    undefined. 

parse_control_frame(V=2, ?SYN_STREAM, Flags, _Length, 
                    << _:1, StreamID:31/big-unsigned-integer,
                       _:1, AssocStreamID:31/big-unsigned-integer,
                       Priority:2/big-unsigned-integer, %% size is 3 in v3
                       _Unused:14/binary-unit:1, %% size is 12 in v3
                       NVPairsData/binary >>) ->
    #spdy_syn_stream{version = V, 
                     flags=Flags,
                     streamid=StreamID,
                     associd=AssocStreamID,
                     priority=Priority,
                     nvdata=NVPairsData};

parse_control_frame(V=2, ?SYN_REPLY, Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                       NVPairsData/binary >> ) ->
    #spdy_syn_reply{version=V,
                    flags=Flags,
                    streamid=StreamID,
                    nvdata=NVPairsData};

parse_control_frame(V=2, ?RST_STREAM, Flags, _Length,
                    <<  _:1, StreamID:31/big-unsigned-integer,
                       StatusCode:32/big-unsigned-integer >>) ->
    #spdy_rst_stream{version=V,
                     flags=Flags,
                     streamid=StreamID,
                     statuscode=StatusCode};

parse_control_frame(V=2, ?SETTINGS, Flags, _Length, Data) ->
    #spdy_settings{ version=V,
                    flags=Flags,
                    settings=parse_settings(Data)};

parse_control_frame(V=2, ?NOOP, _Flags, 0, _Data) ->
    #spdy_noop{ version=V };

parse_control_frame(V=2, ?PING, _Flags, 4, << PingID:32/big-unsigned-integer >>) ->
    #spdy_ping{ version=V, id=PingID };

%% in v3: , StatusCode:32/big-unsigned-integer >>
parse_control_frame(V=2, ?GOAWAY, _Flags, 4,  
                    << _:1, LastGoodStreamID:31/big-unsigned-integer >>) ->
    #spdy_goaway{version=V, lastgoodid=LastGoodStreamID};

parse_control_frame(V=2, ?HEADERS, Flags, _Length,
                     << _:1, StreamID:31/big-unsigned-integer,
                        _Unused:16/binary, %% not in v3?
                        NVPairsData/binary >>) ->
    #spdy_headers{version=V,
                  flags=Flags,
                  streamid=StreamID,
                  nvdata=NVPairsData};

parse_control_frame(_, _, _, _, _) ->
    undefined.

%% Marshal frame back into binary for transmission

build_frame(#spdy_data{streamid=StreamID,
                       flags=Flags, 
                       data=Data}) ->
    Length = size(Data),
    << 0:1, StreamID:31/big-unsigned-integer,
       Flags:8/big-unsigned-integer, 
       Length:24/big-unsigned-integer,
       Data/binary
    >>;   

build_frame(#spdy_syn_stream{version = Version, 
                             flags=Flags,
                             streamid=StreamID,
                             associd=AssocID,
                             priority=Priority,
                             nvdata=NVData}) ->
    bcf(Version, ?SYN_STREAM, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                        0:1, AssocID:31/big-unsigned-integer,
                                        Priority:2/big-unsigned-integer, %% size is 3 in v3
                                        0:14/unit:1, %% size is 12 in v3 (UNUSED)
                                        NVData/binary >>);

build_frame(#spdy_syn_reply{ version = Version, 
                             flags=Flags,
                             streamid=StreamID,
                             nvdata=NVData}) ->
    bcf(Version, ?SYN_REPLY, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                       0:16/unit:1, %% UNUSED
                                       NVData/binary >>);

build_frame(#spdy_rst_stream{version = Version, 
                             flags=Flags,
                             streamid=StreamID,
                             statuscode=StatusCode}) ->
    bcf(Version, ?RST_STREAM, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                        StatusCode:32/big-unsigned-integer >>);

build_frame(#spdy_ping{      version = Version, 
                             id=PingID}) ->
    bcf(Version, ?PING, 0, << PingID:32/big-unsigned-integer >>);

build_frame(#spdy_goaway{    version = Version,
                             lastgoodid=LGI}) ->
    bcf(Version, ?GOAWAY, 0, << 0:1, LGI:31/big-unsigned-integer >>).

%% TODO not implemented build_frame for all types yet


%% Build Control Frame (common header)
bcf(undefined, T,F,D) -> bcf(2,T,F,D);
bcf(Version, Type, Flags, Data) ->
    Length = size(Data),
    << 1:1,
       Version:15/big-unsigned-integer, 
       Type:16/big-unsigned-integer, 
       Flags:8/big-unsigned-integer, 
       Length:24/big-unsigned-integer,
       Data/binary
    >>.

%% 2.6.9 Name/Value Header Block
parse_name_val_pairs(Bin, Z) ->
    Unpacked = unpack(Z, Bin, ?HEADERS_ZLIB_DICT),
    zlib:inflateReset(Z),
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

parse_settings(<<Num:32/big-unsigned-integer, Data/binary>>) ->
    parse_settings_pair(Num, Data, []).

parse_settings_pair(0, _, Acc) -> lists:reverse(Acc);
parse_settings_pair(Num, <<Flags:8/big-unsigned-integer,
                        Id:24/big-unsigned-integer,
                        Value:32/big-unsigned-integer,
                        Rest/binary>>, Acc) ->
    Item = {Id, Flags, Value},
    parse_settings_pair(Num-1, Rest, [Item|Acc]).


unpack(Z, Compressed, Dict) ->
    ?LOG("UNPACK: ~p",[Compressed]),
    case catch zlib:inflate(Z, Compressed) of
         {'EXIT',{{need_dictionary,_DictID},_}} ->
                 zlib:inflateSetDictionary(Z, Dict),
                 iolist_to_binary(zlib:inflate(Z, []));
%%         {'EXIT',{data_error, _}} ->
%%             ?LOG("DATA_ERROR uncompressing ~p",[ Compressed ]),
%%             Compressed;
          Uncompressed ->
                 iolist_to_binary(Uncompressed)
     end.

 %% Various conversions for nicer debugging and matching:
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

type_to_atom(?SYN_STREAM)   -> syn_stream;
type_to_atom(?SYN_REPLY)    -> syn_reply;
type_to_atom(?RST_STREAM)   -> rst_stream;
type_to_atom(?SETTINGS)     -> settings;
type_to_atom(?PING)         -> ping;
type_to_atom(?GOAWAY)       -> goaway;
type_to_atom(?HEADERS)      -> headers;
type_to_atom(?WINDOW_UPDATE)-> window_update;
type_to_atom(X)             -> X.
