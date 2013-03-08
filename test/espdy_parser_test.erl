-module(espdy_parser_test).
-compile(export_all).

% etest macros
-include_lib("eunit/include/eunit.hrl").

% include espdy records
-include("espdy.hrl").

test_basic_operation() ->
    ?assertEqual(1, 1).

%%
%% Control Frame Tests
%%

%% Control Frame Layout:
%% +----------------------------------+
%% |C| Version(15bits) | Type(16bits) |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+

% SETTINGS Control Frame Layout (v2):
% +----------------------------------+
% |1|       2          |       4     |
% +----------------------------------+
% | Flags (8)  |  Length (24 bits)   |
% +----------------------------------+
% |         Number of entries        |
% +----------------------------------+
% |          ID/Value Pairs          |
% |             ...                  |
control_frame_settings_v2_test() ->
    ControlFrameData = <<1:1,                                     % C
                         2:15/big-unsigned-integer,               % Version
                         4:16/big-unsigned-integer,               % Type
                         2:8/big-unsigned-integer,                % Flags
                         12:24/big-unsigned-integer,              % Length size(Data)
                         <<0,0,0,1,0,0,4,1,0,0,0,100>>/binary >>, % Data
    DesiredControlFrame = #spdy_settings{version=2,
                                         flags=2,
                                         settings=[{4, {1, 100}}]},
    {ControlFrame, _Z} = espdy_parser:parse_frame(ControlFrameData, <<>>),
    ?assertEqual(DesiredControlFrame, ControlFrame).

control_frame_settings_v2_raw_test() ->
    ControlFrameData = <<128,2,0,4,0,0,0,12,0,0,0,1,4,0,0,0,0,0,3,232>>,
    DesiredControlFrame = #spdy_settings{version=2,
                                         flags=0,
                                         settings=[{262144,{0,1000}}]},
    {ControlFrame, _Z} = espdy_parser:parse_frame(ControlFrameData, <<>>),
    ?assertEqual(DesiredControlFrame, ControlFrame).

% SETTINGS Control Frame Layout (v3):
% +----------------------------------+
% |1|   version    |         4       |
% +----------------------------------+
% | Flags (8)  |  Length (24 bits)   |
% +----------------------------------+
% |         Number of entries        |
% +----------------------------------+
% |          ID/Value Pairs          |
% |             ...                  |
control_frame_settings_v3_test() ->
    ControlFrameData = <<1:1,                                     % C
                         3:15/big-unsigned-integer,               % Version
                         4:16/big-unsigned-integer,               % Type
                         2:8/big-unsigned-integer,                % Flags
                         12:24/big-unsigned-integer,              % Length size(Data)
                         <<0,0,0,1,0,0,4,1,0,0,0,100>>/binary >>, % Data
    DesiredControlFrame = #spdy_settings{version=3,
                                         flags=2,
                                         settings=[{4, {1, 100}}]},
    {ControlFrame, _Z} = espdy_parser:parse_frame(ControlFrameData, <<>>),
    ?assertEqual(DesiredControlFrame, ControlFrame).

% SYN_STREAM Control Frame Layout (v2):
% +----------------------------------+
% |1|       2          |       1     |
% +----------------------------------+
% | Flags (8)  |  Length (24 bits)   |
% +----------------------------------+
% |X|          Stream-ID (31bits)    |
% +----------------------------------+
% |X|Associated-To-Stream-ID (31bits)|
% +----------------------------------+
% | Pri | Unused    |                |
% +------------------                |
% |     Name/value header block      |
% |             ...                  |
control_frame_syn_stream_v2_raw_test() ->
    ControlFrameData = <<128,2,0,1,1,0,1,34,0,0,0,1,0,0,0,0,0,0,56,234,223,
                         162,81,178,98,224,98,96,131,164,23,6,123,184,11,117,
                         48,44,214,174,64,23,205,205,177,46,180,53,208,179,
                         212,209,210,215,2,179,44,24,248,80,115,44,131,156,
                         103,176,63,212,61,58,96,7,129,213,153,235,64,212,27,
                         51,240,163,229,105,6,65,144,139,117,160,78,214,41,
                         78,73,206,128,171,129,37,3,6,190,212,60,221,208,96,
                         157,212,60,168,165,44,160,60,206,192,7,74,8,57,32,
                         166,149,153,161,145,33,3,91,46,176,108,201,79,97,96,
                         118,119,13,97,96,43,6,38,199,220,84,6,214,140,146,
                         146,130,98,6,102,144,191,25,1,2,72,31,32,128,24,184,
                         16,153,149,161,204,55,191,42,51,39,39,81,223,84,207,
                         64,65,195,55,49,57,51,175,36,191,56,195,90,193,19,
                         152,126,114,20,128,2,10,254,193,10,17,10,134,6,241,
                         22,241,70,154,10,142,192,160,72,13,79,77,242,206,44,
                         209,55,53,54,215,51,54,86,208,240,246,8,241,245,209,
                         81,200,201,204,78,85,112,79,77,206,206,215,84,112,
                         206,0,22,66,169,250,70,230,122,6,122,134,38,198,70,
                         64,195,131,19,211,18,139,50,161,154,24,216,161,81,
                         193,192,1,139,33,0,0,0,0,255,255>>,
    DesiredHeaders = [{<<"accept">>,
                       <<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>},
                      {<<"accept-charset">>,<<"ISO-8859-1,utf-8;q=0.7,*;q=0.3">>},
                      {<<"accept-encoding">>,<<"gzip,deflate,sdch">>},
                      {<<"accept-language">>,<<"en-US,en;q=0.8">>},
                      {<<"host">>,<<"localhost:6121">>},
                      {<<"method">>,<<"GET">>},
                      {<<"scheme">>,<<"https">>},
                      {<<"url">>,<<"/">>},
                      {<<"user-agent">>,
                       <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) ",
                         "AppleWebKit/537.33 (KHTML, like Gecko) ",
                         "Chrome/27.0.1432.0 Safari/537.33">>},
                      {<<"version">>,<<"HTTP/1.1">>}],
    DesiredControlFrame = #spdy_syn_stream{version=2,
                                           flags=1,
                                           streamid=1,
                                           associd=0,
                                           priority=0,
                                           slot=undefined,
                                           headers=DesiredHeaders},
    Zinf = zlib:open(),
    ok = zlib:inflateInit(Zinf),
    {ControlFrame, _Z} = espdy_parser:parse_frame(ControlFrameData, Zinf),
    ?assertEqual(DesiredControlFrame, ControlFrame).

% SYN_STREAM Control Frame Layout (v3):
% +------------------------------------+
% |1|    version    |         1        |
% +------------------------------------+
% |  Flags (8)  |  Length (24 bits)    |
% +------------------------------------+
% |X|           Stream-ID (31bits)     |
% +------------------------------------+
% |X| Associated-To-Stream-ID (31bits) |
% +------------------------------------+
% | Pri|Unused | Slot |                |
% +-------------------+                |
% | Number of Name/Value pairs (int32) |   <+
% +------------------------------------+    |
% |     Length of name (int32)         |    | This section is the "Name/Value
% +------------------------------------+    | Header Block", and is compressed.
% |           Name (string)            |    |
% +------------------------------------+    |
% |     Length of value  (int32)       |    |
% +------------------------------------+    |
% |          Value   (string)          |    |
% +------------------------------------+    |
% |           (repeats)                |   <+
control_frame_syn_stream_v3_test() ->
    RawHeaderData = <<3:32/big-unsigned-integer, % Number of Name/Value Pairs
                      7:32/big-unsigned-integer, % Length of Name (Header 1)
                      <<":method">>/binary,      % Name
                      3:32/big-unsigned-integer, % Length of Value
                      <<"GET">>/binary,          % Value
                      5:32/big-unsigned-integer, % Length of Name (Header 2)
                      <<":path">>/binary,        % Name
                      12:32/big-unsigned-integer,% Length of Value
                      <<"/hello_world">>/binary, % Value
                      8:32/big-unsigned-integer, % Length of Name (Header 3)
                      <<":version">>/binary,     % Name
                      8:32/big-unsigned-integer, % Length of Value
                      <<"HTTP/1.1">>/binary >>,  % Value
    Zdef = zlib:open(),
    ok = zlib:deflateInit(Zdef),
    zlib:deflateSetDictionary(Zdef, ?HEADERS_ZLIB_DICT_V3),
    CompressedHeaderData = iolist_to_binary([
                zlib:deflate(Zdef, RawHeaderData, full)
            ]),

    ?LOG("PACKED: ~p",[CompressedHeaderData]),
    ControlFrameData = <<0:1, 9:31/big-unsigned-integer, % Stream ID
                         0:1, 5:31/big-unsigned-integer, % Associated-To-Stream ID
                         7:3/big-unsigned-integer,       % Priority
                         0:5/big-unsigned-integer,       % Unused
                         0:8/big-unsigned-integer,       % Slot
                         CompressedHeaderData/binary >>, % Compressed Headers
    DataLength = size(ControlFrameData),
    ?LOG("LENGTH: ~p",[DataLength]),
    RawControlFrame = <<1:1,                             % C
                     3:15/big-unsigned-integer,          % Version
                     1:16/big-unsigned-integer,          % Type
                     1:8/big-unsigned-integer,           % Flags
                     DataLength:24/big-unsigned-integer, % Length
                     ControlFrameData/binary >>,         % Data
    ?LOG("CONTROL FRAME: ~p",[RawControlFrame]),

    DesiredHeaders = [{<<":method">>,<<"GET">>},
                      {<<":path">>,<<"/hello_world">>},
                      {<<":version">>,<<"HTTP/1.1">>}],
    DesiredControlFrame = #spdy_syn_stream{version=3,
                                           flags=1,
                                           streamid=9,
                                           associd=5,
                                           priority=7,
                                           slot=0,
                                           headers=DesiredHeaders},
    Zinf = zlib:open(),
    ok = zlib:inflateInit(Zinf),
    {ControlFrame, _Z} = espdy_parser:parse_frame(RawControlFrame, Zinf),
    ?assertEqual(DesiredControlFrame, ControlFrame).

% PING Control Frame Layout (v2/v3):
% +----------------------------------+
% |1|   version    |         6       |
% +----------------------------------+
% | 0 (flags) |     4 (length)       |
% +----------------------------------|
% |            32-bit ID             |
% +----------------------------------+
control_frame_ping_v2_test() ->
    ControlFrameData = <<1:1,                              % C
                         2:15/big-unsigned-integer,        % Version
                         6:16/big-unsigned-integer,        % Type
                         0:8/big-unsigned-integer,         % Flags
                         4:24/big-unsigned-integer,        % Length (fixed)
                         12345:32/big-unsigned-integer >>, % ID
    DesiredControlFrame = #spdy_ping{version=2, id=12345},
    {ControlFrame, _Z} = espdy_parser:parse_frame(ControlFrameData, <<>>),
    ?assertEqual(DesiredControlFrame, ControlFrame).

control_frame_ping_v3_test() ->
    ControlFrameData = <<1:1,                              % C
                         3:15/big-unsigned-integer,        % Version
                         6:16/big-unsigned-integer,        % Type
                         0:8/big-unsigned-integer,         % Flags
                         4:24/big-unsigned-integer,        % Length (fixed)
                         12345:32/big-unsigned-integer >>, % ID
    DesiredControlFrame = #spdy_ping{version=3, id=12345},
    {ControlFrame, _Z} = espdy_parser:parse_frame(ControlFrameData, <<>>),
    ?assertEqual(DesiredControlFrame, ControlFrame).

%% Header encoding tests

encode_name_value_header_v2_test() ->
    Headers = [{<<"method">>,<<"GET">>},
               {<<"url">>,<<"/">>},
               {<<"version">>,<<"HTTP/1.1">>}],
    Desired = <<120,187,223,162,81,178,98,96,102,96,203,5,230,195,252,20,6,102,119,
                215,16,6,102,144,32,163,62,3,59,84,13,3,7,76,43,0,0,0,255,255>>,

    Zdef = zlib:open(),
    ok = zlib:deflateInit(Zdef),
    zlib:deflateSetDictionary(Zdef, ?HEADERS_ZLIB_DICT),
    Packed = espdy_parser:encode_name_value_header(2, Headers, Zdef),
    ?assertEqual(Desired, Packed).

encode_name_value_header_v3_test() ->
    Headers = [{<<":method">>,<<"GET">>},
               {<<":path">>,<<"/hello_world">>},
               {<<":version">>,<<"HTTP/1.1">>}],
    Desired = <<120,187,227,198,167,194,2,37,58,80,122,180,66,164,90,
                119,215,16,80,6,179,42,72,4,151,77,60,250,25,169,192,
                2,50,190,60,191,40,7,156,153,173,176,164,93,0,0,0,0,255,255>>,

    Zdef = zlib:open(),
    ok = zlib:deflateInit(Zdef),
    zlib:deflateSetDictionary(Zdef, ?HEADERS_ZLIB_DICT_V3),
    Packed = espdy_parser:encode_name_value_header(3, Headers, Zdef),
    ?assertEqual(Desired, Packed).