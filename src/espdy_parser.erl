%%
%% Parses frames from wire format to #spdy_* records (parse_frame)
%% and rebuilds wire format from #spdy_* records (build_frame)
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
             >>, _Z) ->
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
            >>, Z) ->
    {parse_control_frame(Version, Type, Flags, Length, Data, Z), Rest};


%% No full frame found:
parse_frame(_Buffer, _Z) ->
    undefined.

parse_control_frame(V=2, ?SYN_STREAM, Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                       _:1, AssocStreamID:31/big-unsigned-integer,
                       Priority:2/big-unsigned-integer,
                       _Unused:14/binary-unit:1,
                       NVPairsData/binary >>, Z) ->
    Headers = parse_name_val_header(V, NVPairsData, Z),
    #spdy_syn_stream{version=V,
                     flags=Flags,
                     streamid=StreamID,
                     associd=AssocStreamID,
                     priority=Priority,
                     headers=Headers};

parse_control_frame(V=3, ?SYN_STREAM, Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                       _:1, AssocStreamID:31/big-unsigned-integer,
                       Priority:3/big-unsigned-integer,
                       _Unused:5/binary-unit:1,
                       Slot:8/big-unsigned-integer,
                       NVPairsData/binary >>, Z) ->
    Headers = parse_name_val_header(V, NVPairsData, Z),
    #spdy_syn_stream{version=V,
                     flags=Flags,
                     streamid=StreamID,
                     associd=AssocStreamID,
                     priority=Priority,
                     slot=Slot,
                     headers=Headers};

parse_control_frame(V=2, ?SYN_REPLY, Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                       _Unused:16/binary-unit:1,
                       NVPairsData/binary >>, Z) ->
    Headers = parse_name_val_header(V, NVPairsData, Z),
    #spdy_syn_reply{version=V,
                    flags=Flags,
                    streamid=StreamID,
                    headers=Headers};

parse_control_frame(V=3, ?SYN_REPLY, Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                       NVPairsData/binary >>, Z) ->
    Headers = parse_name_val_header(V, NVPairsData, Z),
    #spdy_syn_reply{version=V,
                    flags=Flags,
                    streamid=StreamID,
                    headers=Headers};

parse_control_frame(V, ?RST_STREAM, Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                       StatusCode:32/big-unsigned-integer >>, _Z) when V =:= 2; V =:= 3 ->
    #spdy_rst_stream{version=V,
                     flags=Flags,
                     streamid=StreamID,
                     statuscode=StatusCode};

parse_control_frame(V=2, ?SETTINGS, Flags, _Length, Data, _Z) when V =:= 2; V =:= 3 ->
    #spdy_settings{ version=V,
                    flags=Flags,
                    settings=parse_settings(V, Data)};

parse_control_frame(V=3, ?SETTINGS, Flags, _Length, Data, _Z) when V =:= 2; V =:= 3 ->
    #spdy_settings{ version=V,
                    flags=Flags,
                    settings=parse_settings(V, Data)};

% NOOP frame was removed in v3
parse_control_frame(V=2, ?NOOP, _Flags, 0, _Data, _Z) ->
    #spdy_noop{ version=V };

parse_control_frame(V, ?PING, _Flags, 4, << PingID:32/big-unsigned-integer >>, _Z) when V =:= 2; V =:= 3 ->
    #spdy_ping{ version=V, id=PingID };

parse_control_frame(V=2, ?GOAWAY, _Flags, 4,
                    << _:1, LastGoodStreamID:31/big-unsigned-integer >>, _Z) ->
    #spdy_goaway{version=V, lastgoodid=LastGoodStreamID};

parse_control_frame(V=3, ?GOAWAY, _Flags, 8,
                    << _:1,
                       LastGoodStreamID:31/big-unsigned-integer,
                       StatusCode:32/big-unsigned-integer >>, _Z) ->
    #spdy_goaway{version=V, lastgoodid=LastGoodStreamID, statuscode = StatusCode};

parse_control_frame(V=2, ?HEADERS, Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                       _Unused:16/big-unsigned-integer,
                       NVPairsData/binary >>, Z) ->
    parse_headers_frame(V, Flags, StreamID, NVPairsData, Z);

parse_control_frame(V=3, ?HEADERS, Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                       NVPairsData/binary >>, Z) ->
    parse_headers_frame(V, Flags, StreamID, NVPairsData, Z);

parse_control_frame(V=3, ?WINDOW_UPDATE, _Flags, _Length,
                    << _:1, StreamID:31/big-unsigned-integer,
                      _:1, DeltaWindowSize:31 >>, _Z) ->
    #spdy_window_update{version=V,
                        streamid=StreamID,
                        delta_size=DeltaWindowSize};

parse_control_frame(_V, _Type, _Flags, _Len, _Data, _Z) ->
    undefined.

parse_headers_frame(Version, Flags, StreamID, NVPairsData, Z) ->
    Headers = parse_name_val_header(Version, NVPairsData, Z),
    #spdy_headers{version=Version,
                  flags=Flags,
                  streamid=StreamID,
                  headers=Headers}.

%% Marshal frame back into binary for transmission

build_frame(#spdy_data{streamid=StreamID,
                       flags=Flags,
                       data=Data}, _Z) ->
    Length = size(Data),
    << 0:1, StreamID:31/big-unsigned-integer,
       Flags:8/big-unsigned-integer,
       Length:24/big-unsigned-integer,
       Data/binary
    >>;

build_frame(#spdy_syn_stream{version = Version = 2,
                             flags=Flags,
                             streamid=StreamID,
                             associd=AssocID,
                             priority=Priority,
                             headers=Headers}, Z) ->
    NVData = encode_name_value_header(Version, Headers, Z),
    bcf(Version, ?SYN_STREAM, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                        0:1, AssocID:31/big-unsigned-integer,
                                        Priority:2/big-unsigned-integer,
                                        0:14/unit:1, % Unused
                                        NVData/binary >>);

build_frame(#spdy_syn_stream{version = Version = 3,
                             flags=Flags,
                             streamid=StreamID,
                             associd=AssocID,
                             priority=Priority,
                             slot=Slot,
                             headers=Headers}, Z) ->
    NVData = encode_name_value_header(Version, Headers, Z),
    bcf(Version, ?SYN_STREAM, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                        0:1, AssocID:31/big-unsigned-integer,
                                        Priority:3/big-unsigned-integer,
                                        0:5/unit:1, % Unused
                                        Slot:8/big-unsigned-integer,
                                        NVData/binary >>);

build_frame(#spdy_headers{   version = Version = 2,
                             flags=Flags,
                             streamid=StreamID,
                             headers=Headers}, Z) ->
    NVData = encode_name_value_header(Version, Headers, Z),
    bcf(Version, ?HEADERS, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                     0:16/unit:1, %% UNUSED
                                     NVData/binary >>);

build_frame(#spdy_headers{   version = Version = 3,
                             flags=Flags,
                             streamid=StreamID,
                             headers=Headers}, Z) ->
    NVData = encode_name_value_header(Version, Headers, Z),
    bcf(Version, ?HEADERS, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                     NVData/binary >>);

build_frame(#spdy_syn_reply{ version = Version = 2,
                             flags=Flags,
                             streamid=StreamID,
                             headers=Headers}, Z) ->
    NVData = encode_name_value_header(Version, Headers, Z),
    bcf(Version, ?SYN_REPLY, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                       0:16/unit:1, %% UNUSED
                                       NVData/binary >>);

build_frame(#spdy_syn_reply{ version = Version = 3,
                             flags=Flags,
                             streamid=StreamID,
                             headers=Headers}, Z) ->
    NVData = encode_name_value_header(Version, Headers, Z),
    bcf(Version, ?SYN_REPLY, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                       NVData/binary >>);

build_frame(#spdy_rst_stream{version = Version,
                             flags=Flags,
                             streamid=StreamID,
                             statuscode=StatusCode}, _Z) ->
    bcf(Version, ?RST_STREAM, Flags, << 0:1, StreamID:31/big-unsigned-integer,
                                        StatusCode:32/big-unsigned-integer >>);

build_frame(#spdy_ping{      version = Version,
                             id=PingID}, _Z) ->
    bcf(Version, ?PING, 0, << PingID:32/big-unsigned-integer >>);

build_frame(#spdy_goaway{    version = Version = 2,
                             lastgoodid=LGI}, _Z) ->
    bcf(Version, ?GOAWAY, 0, << 0:1, LGI:31/big-unsigned-integer >>);

build_frame(#spdy_goaway{    version = Version = 3,
                             lastgoodid=LGI,
                             statuscode=StatusCode}, _Z) ->
    bcf(Version, ?GOAWAY, 0, << 0:1, LGI:31/big-unsigned-integer,
                                StatusCode:32/big-unsigned-integer >>);

build_frame(#spdy_settings{  version = Version,
                             flags = Flags,
                             settings = Settings }, _Z) ->
    bcf(Version, ?SETTINGS, Flags, encode_settings(Version, Settings));

build_frame(#spdy_window_update{version = Version = 3,
                                streamid=StreamID,
                                delta_size=DeltaWindowSize}, _Z) ->
    bcf(Version, ?WINDOW_UPDATE, 0, << 0:1, StreamID:31/big-unsigned-integer,
                                       0:1, DeltaWindowSize:31/big-unsigned-integer >>).

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
parse_name_val_header(Version = 2, Bin, Z) ->
    Unpacked = unpack(Z, Bin, ?HEADERS_ZLIB_DICT),
    <<Num:16/big-unsigned-integer, Rest/binary>> = Unpacked,
    parse_name_val_pairs(Version, Num, Rest, []);

%% 2.6.10 Name/Value Header Block
parse_name_val_header(Version = 3, Bin, Z) ->
    Unpacked = unpack(Z, Bin, ?HEADERS_ZLIB_DICT_V3),
    <<Num:32/big-unsigned-integer, Rest/binary>> = Unpacked,
    parse_name_val_pairs(Version, Num, Rest, []).

parse_name_val_pairs(_V, 0, _, Acc) ->
    lists:reverse(Acc);

%% Don't allow 0-length header names
parse_name_val_pairs(_Version = 2, _Num, << 0:16/big-unsigned-integer,
                     ValLen:16/big-unsigned-integer,
                     _Val:ValLen/binary,
                     _Rest/binary >>, _Acc) ->
    {error, stream_protocol_error};
%% Don't allow 0-length header values
parse_name_val_pairs(_Version = 2, _Num, << NameLen:16/big-unsigned-integer,
                     _Name:NameLen/binary,
                     0:16/big-unsigned-integer,
                     _Rest/binary >>, _Acc) ->
    {error, stream_protocol_error};
parse_name_val_pairs(Version = 2, Num, << NameLen:16/big-unsigned-integer,
                     Name:NameLen/binary,
                     ValLen:16/big-unsigned-integer,
                     Val:ValLen/binary,
                     Rest/binary >>, Acc) ->
    case validate_header_name(Version, Name) of
        % Unsure about this, spec is not clear
        invalid -> {error, stream_protocol_error};
        ok ->
            case binary:match(Val, <<0>>) of
                nomatch ->
                    Pair = {Name, Val},
                    parse_name_val_pairs(Version, Num-1, Rest, [Pair | Acc]);
                _ ->
                    %% No consecutive NULs in value
                    case binary:match(Val, <<0,0>>) of
                        nomatch ->
                            %% Split multiple header values on NUL
                            Pair = {Name, binary:split(Val, <<0>>, [global])},
                            parse_name_val_pairs(Version, Num-1, Rest, [Pair | Acc]);
                        _ ->
                            %% Don't allow consecutive nuls
                            {error, stream_protocol_error}
                    end
            end
    end;

parse_name_val_pairs(Version = 3, Num, << NameLen:32/big-unsigned-integer,
                     Name:NameLen/binary,
                     ValLen:32/big-unsigned-integer,
                     Val:ValLen/binary,
                     Rest/binary >>, Acc) ->
    %% TODO validate and throw errors as per 2.6.10. To validate:
    %% * Names must be lowercase ASCII
    %% * Length of each name must be > 0
    %% * Unique per header block, no duplicates
    %% * Values must be either empty (length=0) or contain multiple,
    %%   NUL-separated values, each with length > 0. No in-sequence NUL chars.
    %% * Values cannot start or end with a NUL character
    %% * Not sure on charset for Values (also ASCII ?)
    %%
    %% If any of these validations fail, issue a stream error with status code
    %% PROTOCOL_ERROR with the stream-id.
    Pair = {Name, Val},
    parse_name_val_pairs(Version, Num-1, Rest, [Pair | Acc]).

validate_header_name(_Version = 2, <<>>) -> invalid;
validate_header_name(Version = 2, <<C:8>>) ->
    validate_header_char(Version, C);
validate_header_name(Version = 2, <<C:8, Rest/binary>>) ->
    case validate_header_char(Version, C) of
        invalid -> invalid;
        ok -> validate_header_name(Version, Rest)
    end.

validate_header_char(2, 0) ->
    invalid; %% NUL is not allowed
validate_header_char(2, C) when is_integer(C), C > 64, C < 91 ->
    invalid; %% uppercase chars not allowed
validate_header_char(2, C) when is_integer(C), C > 127 ->
    invalid; %% Invalid ASCII range
validate_header_char(2, C) when is_integer(C) ->
    ok.

%% END nvpair stuff

parse_settings(Version, <<Num:32/big-unsigned-integer, Data/binary>>) ->
    parse_settings_pair(Version, Num, Data, []).

parse_settings_pair(_Version, _Num = 0, _, Acc) -> lists:reverse(Acc);

parse_settings_pair(Version = 2, Num, <<Id:24/little-unsigned-integer, % Bug in the draft2 spec
                                        Flags:8/big-unsigned-integer,
                                        Value:32/big-unsigned-integer,
                                        Rest/binary>>, Acc) ->
    Item = #spdy_setting_pair{id=Id, flags=Flags, value=Value},
    parse_settings_pair(Version, Num-1, Rest, [Item|Acc]);

parse_settings_pair(Version = 3, Num, <<Flags:8/big-unsigned-integer,
                                        Id:24/big-unsigned-integer,
                                        Value:32/big-unsigned-integer,
                                        Rest/binary>>, Acc) ->
    Item = #spdy_setting_pair{id=Id, flags=Flags, value=Value},
    parse_settings_pair(Version, Num-1, Rest, [Item|Acc]).

%% [{key, {flags, val}}..]
encode_settings(Version, Settings) ->
    {Num, Bin} = encode_settings(Version, Settings, <<>>, 0),
    << Num:32/big-unsigned-integer, Bin/binary>>.

encode_settings(_Version, [], Acc, Num) ->
    {Num, Acc};
encode_settings(Version = 2,
                [#spdy_setting_pair{id = Id, flags = Flags, value = Val}|Rest],
                Acc,
                Num) ->
    Item = << Id:24/little-unsigned-integer, % Bug in the draft2 spec
              Flags:8/big-unsigned-integer,
              Val:32/big-unsigned-integer >>,
    encode_settings(Version, Rest, << Acc/binary, Item/binary >>, Num+1);
encode_settings(Version = 3,
                [#spdy_setting_pair{id = Id, flags = Flags, value = Val}|Rest],
                Acc,
                Num) ->
    Item = << Flags:8/big-unsigned-integer,
              Id:24/big-unsigned-integer,
              Val:32/big-unsigned-integer >>,
    encode_settings(Version, Rest, << Acc/binary, Item/binary >>, Num+1).



unpack(Z, Compressed, Dict) ->
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

%% Encode the name/value compressed header block
encode_name_value_header(_Version = 2, Headers, Z) when is_list(Headers) ->
    Num = length(Headers),
    L = lists:foldl(fun({K,V}, Acc) ->
        Klen = size(K),
        Vlen = size(V),
        [ <<Klen:16/unsigned-big-integer, K/binary, Vlen:16/unsigned-big-integer, V/binary>> | Acc ]
    end, [<< Num:16/unsigned-big-integer >>], Headers),
    ToDeflate = iolist_to_binary(lists:reverse(L)),
    Deflated = iolist_to_binary([
            zlib:deflate(Z, ToDeflate, full)
        ]),
    %%io:format("deflated: ~p\n",[Deflated]),
    %%zlib:close(Z),
    Deflated;
encode_name_value_header(_Version = 3, Headers, Z) when is_list(Headers) ->
    Num = length(Headers),
    L = lists:foldl(fun({K,V}, Acc) ->
        Klen = size(K),
        Vlen = size(V),
        [ <<Klen:32/unsigned-big-integer, K/binary, Vlen:32/unsigned-big-integer, V/binary>> | Acc ]
    end, [<< Num:32/unsigned-big-integer >>], Headers),
    ToDeflate = iolist_to_binary(lists:reverse(L)),
    Deflated = iolist_to_binary([
            zlib:deflate(Z, ToDeflate, full)
        ]),
    Deflated.


 %% Various conversions for nicer debugging and matching:
atom_to_status_code(protocol_error)        -> 1;
atom_to_status_code(invalid_stream)        -> 2;
atom_to_status_code(refused_stream)        -> 3;
atom_to_status_code(unsupported_version)   -> 4;
atom_to_status_code(cancel)                -> 5;
atom_to_status_code(internal_error)        -> 6;
atom_to_status_code(flow_control_error)    -> 7;
atom_to_status_code(stream_in_use)         -> 8;
atom_to_status_code(stream_already_closed) -> 9;
atom_to_status_code(invalid_credentials)   -> 10;
atom_to_status_code(frame_too_large)       -> 11.

status_code_to_atom(1)  -> protocol_error;
status_code_to_atom(2)  -> invalid_stream;
status_code_to_atom(3)  -> refused_stream;
status_code_to_atom(4)  -> unsupported_version;
status_code_to_atom(5)  -> cancel;
status_code_to_atom(6)  -> internal_error;
status_code_to_atom(7)  -> flow_control_error;
status_code_to_atom(8)  -> stream_in_use;
status_code_to_atom(9)  -> stream_already_closed;
status_code_to_atom(10) -> invalid_credentials;
status_code_to_atom(11) -> frame_too_large.

type_to_atom(?SYN_STREAM)   -> syn_stream;
type_to_atom(?SYN_REPLY)    -> syn_reply;
type_to_atom(?RST_STREAM)   -> rst_stream;
type_to_atom(?SETTINGS)     -> settings;
type_to_atom(?PING)         -> ping;
type_to_atom(?GOAWAY)       -> goaway;
type_to_atom(?HEADERS)      -> headers;
type_to_atom(?WINDOW_UPDATE)-> window_update;
type_to_atom(X)             -> X.
