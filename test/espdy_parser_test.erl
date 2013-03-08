-module(espdy_parser_test).
-compile(export_all).

% etest macros
-include_lib("eunit/include/eunit.hrl").

% include espdy records
-include("espdy.hrl").

test_basic_operation() ->
    ?assertEqual(1, 1).


% +----------------------------------+
% |C| Version(15bits) | Type(16bits) |
% +----------------------------------+
% | Flags (8)  |  Length (24 bits)   |
% +----------------------------------+
% |               Data               |
% +----------------------------------+
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
    ?assertEqual(ControlFrame, DesiredControlFrame).

control_frame_settings_v2_raw_test() ->
    ControlFrameData = <<128,2,0,4,0,0,0,12,0,0,0,1,4,0,0,0,0,0,3,232>>,
    DesiredControlFrame = #spdy_settings{version=2,
                                         flags=0,
                                         settings=[{262144,{0,1000}}]},
    {ControlFrame, _Z} = espdy_parser:parse_frame(ControlFrameData, <<>>),
    ?assertEqual(DesiredControlFrame, ControlFrame).

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
                                           headers=DesiredHeaders},
    Zinf = zlib:open(),
    ok = zlib:inflateInit(Zinf),
    {ControlFrame, _Z} = espdy_parser:parse_frame(ControlFrameData, Zinf),
    ?assertEqual(DesiredControlFrame, ControlFrame).
