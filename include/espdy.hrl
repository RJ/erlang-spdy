-define(LOG(S,A), io:format("~p\t" ++ S ++"\n",[self()|A])).

%% only support this version atm
%% in future, we'll use the version claimed by the client and remove this:
-define(SPDY_VERSION, 2).

%% DATA FRAMES:
-record(spdy_data, {   
    streamid :: integer(), 
    flags = 0 :: integer(),
    data :: binary()
}).

%% CONTROL FRAMES:
-record(spdy_syn_stream, {
    version = 2 :: integer(),
    flags = 0   :: integer(),
    streamid    :: integer(),
    associd     :: integer(),
    priority    :: integer(),
    slot        :: integer(),
    headers     :: list()
}).
-record(spdy_syn_reply, {
    version = 2 :: integer(), 
    flags = 0   :: integer(),
    streamid    :: integer(),
    headers     :: list()
}).
-record(spdy_rst_stream, {
    version = 2 :: integer(), 
    flags = 0   :: integer(),
    streamid    :: integer(),
    statuscode  :: integer()
}).
-record(spdy_settings, {
    version = 2 :: integer(), 
    flags = 0   :: integer(),
    settings    :: list()
}).
-record(spdy_noop, {
    version = 2 :: integer()
}).
-record(spdy_ping, {
    version = 2 :: integer(), 
    id          :: integer()
}).
-record(spdy_goaway, {
    version = 2 :: integer(), 
    lastgoodid  :: integer()
}).
-record(spdy_headers, {
    version = 2 :: integer(), 
    flags = 0   :: integer(),
    streamid    :: integer(),
    headers     :: list()
}).

%% STREAMS
-record(stream, {
    id :: integer(), %% If the server is initiating the stream, the Stream-ID must be even. 
                     %% If the client is initiating the stream, the Stream-ID must be odd. 
                     %% 0 is not a valid Stream-ID.
    pid,
    associd = 0 :: integer(),
    headers = [] :: list(), %% Streams optionally carry a set of name/value header pairs.
%%    clientclosed = false, %% them
%%    serverclosed = false, %% us
    priority :: integer(), %% The creator of a stream assigns a priority for that stream. 
                           %% Priority is represented as an integer from 0 to 7. 
                           %% 0 represents the highest priority and 7 represents the lowest priority.
    syn_replied = false :: boolean(), %% true once syn_reply was seen/sent
    window = 64*1024 :: integer() %% default transmit window size is 64KB
}).

%% CONSTANTS
-define(SYN_STREAM, 1).
-define(SYN_REPLY, 2).
-define(RST_STREAM, 3).
-define(SETTINGS, 4).
-define(NOOP, 5).
-define(PING, 6).
-define(GOAWAY, 7).
-define(HEADERS, 8).
-define(WINDOW_UPDATE, 9).
-define(CONTROL_FLAG_NONE, 0).
-define(CONTROL_FLAG_FIN, 1).
-define(CONTROL_FLAG_UNIDIRECTIONAL, 2).
-define(DATA_FLAG_NONE, 0).
-define(DATA_FLAG_FIN, 1).
-define(DATA_FLAG_COMPRESSED, 2).
-define(PROTOCOL_ERROR, 1).
-define(INVALID_STREAM, 1).
-define(REFUSED_STREAM, 2).
-define(UNSUPPORTED_VERSION, 4).
-define(CANCEL, 5).
-define(INTERNAL_ERROR, 6).
-define(FLOW_CONTROL_ERROR, 7).
-define(SETTINGS_UPLOAD_BANDWIDTH, 1).
-define(SETTINGS_DOWNLOAD_BANDWIDTH, 2).
-define(SETTINGS_ROUND_TRIP_TIME, 3).
-define(SETTINGS_MAX_CONCURRENT_STREAMS, 4).
-define(SETTINGS_CURRENT_CWND, 5).
-define(SETTINGS_FLAG_PERSIST_VALUE, 1).
-define(SETTINGS_FLAG_PERSISTED, 2).
-define(SETTINGS_FLAG_CLEAR_PREVIOUSLY_PERSISTED_SETTINGS, 1).

%% The entire contents of the name/value header block is compressed using zlib deflate.  
%% There is a single zlib stream (context) for all name value pairs in one direction on a connection
-define(HEADERS_ZLIB_DICT, <<"optionsgetheadpostputdeletetraceacceptaccept-charsetaccept-encodingaccept-languageauthorizationexpectfromhostif-modified-sinceif-matchif-none-matchif-rangeif-unmodifiedsincemax-forwardsproxy-authorizationrangerefererteuser-agent100101200201202203204205206300301302303304305306307400401402403404405406407408409410411412413414415416417500501502503504505accept-rangesageetaglocationproxy-authenticatepublicretry-afterservervarywarningwww-authenticateallowcontent-basecontent-encodingcache-controlconnectiondatetrailertransfer-encodingupgradeviawarningcontent-languagecontent-lengthcontent-locationcontent-md5content-rangecontent-typeetagexpireslast-modifiedset-cookieMondayTuesdayWednesdayThursdayFridaySaturdaySundayJanFebMarAprMayJunJulAugSepOctNovDecchunkedtext/htmlimage/pngimage/jpgimage/gifapplication/xmlapplication/xhtmltext/plainpublicmax-agecharset=iso-8859-1utf-8gzipdeflateHTTP/1.1statusversionurl",0>>).

-define(HEADERS_ZLIB_DICT_V3, <<
    0,0,0,7,111,112,116,105,111,110,115,0,0,0,4,104,101,97,100,0,
    0,0,4,112,111,115,116,0,0,0,3,112,117,116,0,0,0,6,100,101,
    108,101,116,101,0,0,0,5,116,114,97,99,101,0,0,0,6,97,99,99,
    101,112,116,0,0,0,14,97,99,99,101,112,116,45,99,104,97,114,115,101,
    116,0,0,0,15,97,99,99,101,112,116,45,101,110,99,111,100,105,110,103,
    0,0,0,15,97,99,99,101,112,116,45,108,97,110,103,117,97,103,101,0,
    0,0,13,97,99,99,101,112,116,45,114,97,110,103,101,115,0,0,0,3,
    97,103,101,0,0,0,5,97,108,108,111,119,0,0,0,13,97,117,116,104,
    111,114,105,122,97,116,105,111,110,0,0,0,13,99,97,99,104,101,45,99,
    111,110,116,114,111,108,0,0,0,10,99,111,110,110,101,99,116,105,111,110,
    0,0,0,12,99,111,110,116,101,110,116,45,98,97,115,101,0,0,0,16,
    99,111,110,116,101,110,116,45,101,110,99,111,100,105,110,103,0,0,0,16,
    99,111,110,116,101,110,116,45,108,97,110,103,117,97,103,101,0,0,0,14,
    99,111,110,116,101,110,116,45,108,101,110,103,116,104,0,0,0,16,99,111,
    110,116,101,110,116,45,108,111,99,97,116,105,111,110,0,0,0,11,99,111,
    110,116,101,110,116,45,109,100,53,0,0,0,13,99,111,110,116,101,110,116,
    45,114,97,110,103,101,0,0,0,12,99,111,110,116,101,110,116,45,116,121,
    112,101,0,0,0,4,100,97,116,101,0,0,0,4,101,116,97,103,0,0,
    0,6,101,120,112,101,99,116,0,0,0,7,101,120,112,105,114,101,115,0,
    0,0,4,102,114,111,109,0,0,0,4,104,111,115,116,0,0,0,8,105,
    102,45,109,97,116,99,104,0,0,0,17,105,102,45,109,111,100,105,102,105,
    101,100,45,115,105,110,99,101,0,0,0,13,105,102,45,110,111,110,101,45,
    109,97,116,99,104,0,0,0,8,105,102,45,114,97,110,103,101,0,0,0,
    19,105,102,45,117,110,109,111,100,105,102,105,101,100,45,115,105,110,99,101,
    0,0,0,13,108,97,115,116,45,109,111,100,105,102,105,101,100,0,0,0,
    8,108,111,99,97,116,105,111,110,0,0,0,12,109,97,120,45,102,111,114,
    119,97,114,100,115,0,0,0,6,112,114,97,103,109,97,0,0,0,18,112,
    114,111,120,121,45,97,117,116,104,101,110,116,105,99,97,116,101,0,0,0,
    19,112,114,111,120,121,45,97,117,116,104,111,114,105,122,97,116,105,111,110,
    0,0,0,5,114,97,110,103,101,0,0,0,7,114,101,102,101,114,101,114,
    0,0,0,11,114,101,116,114,121,45,97,102,116,101,114,0,0,0,6,115,
    101,114,118,101,114,0,0,0,2,116,101,0,0,0,7,116,114,97,105,108,
    101,114,0,0,0,17,116,114,97,110,115,102,101,114,45,101,110,99,111,100,
    105,110,103,0,0,0,7,117,112,103,114,97,100,101,0,0,0,10,117,115,
    101,114,45,97,103,101,110,116,0,0,0,4,118,97,114,121,0,0,0,3,
    118,105,97,0,0,0,7,119,97,114,110,105,110,103,0,0,0,16,119,119,
    119,45,97,117,116,104,101,110,116,105,99,97,116,101,0,0,0,6,109,101,
    116,104,111,100,0,0,0,3,103,101,116,0,0,0,6,115,116,97,116,117,
    115,0,0,0,6,50,48,48,32,79,75,0,0,0,7,118,101,114,115,105,
    111,110,0,0,0,8,72,84,84,80,47,49,46,49,0,0,0,3,117,114,
    108,0,0,0,6,112,117,98,108,105,99,0,0,0,10,115,101,116,45,99,
    111,111,107,105,101,0,0,0,10,107,101,101,112,45,97,108,105,118,101,0,
    0,0,6,111,114,105,103,105,110,49,48,48,49,48,49,50,48,49,50,48,
    50,50,48,53,50,48,54,51,48,48,51,48,50,51,48,51,51,48,52,51,
    48,53,51,48,54,51,48,55,52,48,50,52,48,53,52,48,54,52,48,55,
    52,48,56,52,48,57,52,49,48,52,49,49,52,49,50,52,49,51,52,49,
    52,52,49,53,52,49,54,52,49,55,53,48,50,53,48,52,53,48,53,50,
    48,51,32,78,111,110,45,65,117,116,104,111,114,105,116,97,116,105,118,101,
    32,73,110,102,111,114,109,97,116,105,111,110,50,48,52,32,78,111,32,67,
    111,110,116,101,110,116,51,48,49,32,77,111,118,101,100,32,80,101,114,109,
    97,110,101,110,116,108,121,52,48,48,32,66,97,100,32,82,101,113,117,101,
    115,116,52,48,49,32,85,110,97,117,116,104,111,114,105,122,101,100,52,48,
    51,32,70,111,114,98,105,100,100,101,110,52,48,52,32,78,111,116,32,70,
    111,117,110,100,53,48,48,32,73,110,116,101,114,110,97,108,32,83,101,114,
    118,101,114,32,69,114,114,111,114,53,48,49,32,78,111,116,32,73,109,112,
    108,101,109,101,110,116,101,100,53,48,51,32,83,101,114,118,105,99,101,32,
    85,110,97,118,97,105,108,97,98,108,101,74,97,110,32,70,101,98,32,77,
    97,114,32,65,112,114,32,77,97,121,32,74,117,110,32,74,117,108,32,65,
    117,103,32,83,101,112,116,32,79,99,116,32,78,111,118,32,68,101,99,32,
    48,48,58,48,48,58,48,48,32,77,111,110,44,32,84,117,101,44,32,87,
    101,100,44,32,84,104,117,44,32,70,114,105,44,32,83,97,116,44,32,83,
    117,110,44,32,71,77,84,99,104,117,110,107,101,100,44,116,101,120,116,47,
    104,116,109,108,44,105,109,97,103,101,47,112,110,103,44,105,109,97,103,101,
    47,106,112,103,44,105,109,97,103,101,47,103,105,102,44,97,112,112,108,105,
    99,97,116,105,111,110,47,120,109,108,44,97,112,112,108,105,99,97,116,105,
    111,110,47,120,104,116,109,108,43,120,109,108,44,116,101,120,116,47,112,108,
    97,105,110,44,116,101,120,116,47,106,97,118,97,115,99,114,105,112,116,44,
    112,117,98,108,105,99,112,114,105,118,97,116,101,109,97,120,45,97,103,101,
    61,103,122,105,112,44,100,101,102,108,97,116,101,44,115,100,99,104,99,104,
    97,114,115,101,116,61,117,116,102,45,56,99,104,97,114,115,101,116,61,105,
    115,111,45,56,56,53,57,45,49,44,117,116,102,45,44,42,44,101,110,113,
    61,48,46 >>).
