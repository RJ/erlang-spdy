%%
%% Callback module that should respond to (HTTP) reqs incoming via SPDY
%%
%% Each SPDY stream creates a process, this module is invoked once per 
%% stream.
%%
%% If you implement one of these specific to a webserver library, you could
%% marshal the SPDY request to a normal looking HTTP request, and reuse your
%% normal HTTP response / handling code. 
%%
%% See github.com/RJ/cowboy for an example of this.
%% By starting a SPDY listener, the same code that handles HTTP requests is also
%% used to service SPDY requests.
%%
%% This is an example callback module which just returns "Hello SPDY World"
%%
-module(espdy_stream_http).
%%-behaviour(espdy_stream).
-include("include/espdy.hrl").

-export([init/3, closed/2, headers_updated/3, handle_data/2]). %% API

% header name for spdy version
hfv(scheme,  _Version = 2) -> <<"scheme">>;
hfv(status,  _Version = 2) -> <<"status">>;
hfv(host,    _Version = 2) -> <<"host">>;
hfv(method,  _Version = 2) -> <<"method">>;
hfv(path,    _Version = 2) -> <<"url">>;
hfv(version, _Version = 2) -> <<"version">>;

hfv(scheme,  _Version = 3) -> <<":scheme">>;
hfv(status,  _Version = 3) -> <<":status">>;
hfv(host,    _Version = 3) -> <<":host">>;
hfv(method,  _Version = 3) -> <<":method">>;
hfv(path,    _Version = 3) -> <<":path">>;
hfv(version, _Version = 3) -> <<":version">>;

hfv(Name, _) -> atom_to_list(Name).

init(_Id, Headers, SpdyOpts) ->
    ?LOG("INIT opts ~p",[SpdyOpts]),
    SpdyVersion = proplists:get_value(spdy_version, SpdyOpts),
    Scheme = proplists:get_value(hfv(scheme, SpdyVersion), Headers), %% http | https
    Host = proplists:get_value(hfv(host, SpdyVersion), Headers),     %% localhost:6121
    Method = proplists:get_value(hfv(method, SpdyVersion), Headers), %% GET
    Url = proplists:get_value(hfv(path, SpdyVersion), Headers),      %% /
    %% if any of these fields are missing, it's an error
    io:format("scheme=~p host=~p method=~p url=~p~n", [Scheme, Host, Method, Url]),
    case Scheme =:= undefined orelse
         Host   =:= undefined orelse
         Method =:= undefined orelse
         Url    =:= undefined of
        true -> 
            {error, not_http};
        false ->
            case Url of
                <<"/settings-me">> ->
                    %% invent some settings to save
                    Settings = [#spdy_setting_pair{id=?SETTINGS_ROUND_TRIP_TIME,
                                                   flags=?SETTINGS_FLAG_PERSIST_VALUE,
                                                   value=42},
                                #spdy_setting_pair{id=?SETTINGS_MAX_CONCURRENT_STREAMS,
                                                   flags=?SETTINGS_FLAG_PERSIST_VALUE,
                                                   value=9999}
                               ],
                    F = #spdy_settings{version=SpdyVersion,
                                       flags=?SETTINGS_FLAG_CLEAR_PREVIOUSLY_PERSISTED_SETTINGS,
                                       settings=Settings},
                    espdy_stream:send_frame(self(), F),
                    ResponseHeaders = [
                        {hfv(status, SpdyVersion), <<"200 OK">>},
                        {hfv(version, SpdyVersion), <<"HTTP/1.1">>},
                        {<<"content-type">>, <<"text/plain">>}
                    ],
                    Body = <<"This stream sent a settings frame, informing your browser that the server supports 9999 max concurrent streams">>,
                    {ok, ResponseHeaders, Body};

                <<"/">> ->

                    %% Ignore the request, and just return a static page response:
                    ResponseHeaders = [
                        {hfv(status, SpdyVersion), <<"200 OK">>},
                        {hfv(version, SpdyVersion), <<"HTTP/1.1">>},
                        {<<"content-type">>, <<"text/plain">>},
                        {<<"content-length">>, <<"16">>}
                    ],
                    Body = <<"Hello SPDY World">>,
                    {ok, ResponseHeaders, Body};
                _ ->
                    ResponseHeaders = [
                        {hfv(status, SpdyVersion), <<"400 Not Found">>},
                        {hfv(version, SpdyVersion), <<"HTTP/1.1">>},
                        {<<"content-type">>, <<"text/html">>},
                        {<<"content-length">>, <<"16">>}
                    ],
                    Body = <<"<h1>NOT FOUND</h1>">>,
                    {ok, ResponseHeaders, Body}
            end
    end.

%% Called when the SPDY session terminates
closed(Reason, _State) ->
    io:format("CLOSED! ~p\n",[Reason]).

%% Called when a HEADERS frame updated the headers
headers_updated(_Delta, _NewMergedHeaders, State) ->
    io:format("headers updated with ~p",[_Delta]),
    {ok, State}.

%% Called when we recieve a DATA frame
handle_data(Data, State) ->
    io:format("DATA on stream ~p",[Data]),
    {ok, State}.

