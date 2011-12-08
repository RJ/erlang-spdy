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

init(_Id, Headers, SpdyOpts) ->
    ?LOG("INIT opts ~p",[SpdyOpts]),
    Scheme = proplists:get_value(<<"scheme">>, Headers), %% http | https
    Host = proplists:get_value(<<"host">>, Headers),     %% localhost:6121
    Method = proplists:get_value(<<"method">>, Headers), %% GET
    Url = proplists:get_value(<<"url">>, Headers),       %% /
    %% if any of these fields are missing, it's an error
    case Scheme =:= undefined orelse
         Host   =:= undefined orelse
         Method =:= undefined orelse
         Url    =:= undefined of
        true -> 
            {error, not_http};
        false ->
            case Url of
                <<"/">> ->
                    %% Ignore the request, and just return a static page response:
                    ResponseHeaders = [ 
                        {<<"url">>, <<"http://localhost:6121/">>}, %% url only needed in push streams usually?
                        {<<"status">>, <<"200 OK">>},
                        {<<"version">>, <<"HTTP/1.1">>},
                        {<<"content-type">>, <<"text/plain">>},
                        {<<"content-length">>, <<"16">>}
                    ],
                    Body = <<"Hello SPDY World">>,
                    {ok, ResponseHeaders, Body};
                _ ->
                    ResponseHeaders = [ 
                        {<<"status">>, <<"400 Not Found">>},
                        {<<"version">>, <<"HTTP/1.1">>},
                        {<<"content-type">>, <<"text/html">>}
%                        {<<"content-length">>, <<"16">>}
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

