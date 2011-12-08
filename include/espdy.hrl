-define(LOG(S,A), io:format("~p\t" ++ S ++"\n",[self()|A])).

%% only support this version atm
%% in future, we'll use the version claimed by the client and remove this:
-define(SPDY_VERSION, 2).

%% The entire contents of the name/value header block is compressed using zlib deflate.  
%% There is a single zlib stream (context) for all name value pairs in one direction on a connection
-define(HEADERS_ZLIB_DICT, <<"optionsgetheadpostputdeletetraceacceptaccept-charsetaccept-encodingaccept-languageauthorizationexpectfromhostif-modified-sinceif-matchif-none-matchif-rangeif-unmodifiedsincemax-forwardsproxy-authorizationrangerefererteuser-agent100101200201202203204205206300301302303304305306307400401402403404405406407408409410411412413414415416417500501502503504505accept-rangesageetaglocationproxy-authenticatepublicretry-afterservervarywarningwww-authenticateallowcontent-basecontent-encodingcache-controlconnectiondatetrailertransfer-encodingupgradeviawarningcontent-languagecontent-lengthcontent-locationcontent-md5content-rangecontent-typeetagexpireslast-modifiedset-cookieMondayTuesdayWednesdayThursdayFridaySaturdaySundayJanFebMarAprMayJunJulAugSepOctNovDecchunkedtext/htmlimage/pngimage/jpgimage/gifapplication/xmlapplication/xhtmltext/plainpublicmax-agecharset=iso-8859-1utf-8gzipdeflateHTTP/1.1statusversionurl",0>>).

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
