-module(espdy_demo_server_ssl).
-export([start/1]).

%% This is not a scalable way to accept tcp connections - it's just a demo
%% Do not benchmark the server using this code:
start(Port) when is_integer(Port) ->
    ok = ssl:start(),
    spawn_link(fun() ->
        {ok, Listen} = ssl:listen(Port, [
            %% Requires patched SSL:
            {next_protocols_advertised, [<<"spdy/2">>, <<"http/1.0">>, <<"http/1.1">>]},
            {keyfile, "server.key"},
            {certfile, "server.crt"},
%           {cacertfile, "gd_bundle.crt"},
            {reuseaddr, true},
            {ip, {127,0,0,1}},
            binary,
            {packet, raw},
            {active, false},
            {depth,9}
        ]),
        io:format("Spdy demo server listening on https://localhost:~B/\n",[Port]),
        accept_loop(Listen)
    end).

accept_loop(ListenSock) ->
    case ssl:transport_accept(ListenSock) of
        {ok, Sock} ->
            case ssl:ssl_accept(Sock) of 
                ok ->
                    io:format("Negotationed next protocol: ~p\n",[ssl:negotiated_next_protocol(Sock)]),
                    Opts = [], %% passed through to the stream callback module
                    {ok, Pid} = espdy_session:start_link(Sock, ssl, espdy_stream_http, Opts),
                    %% You must assign controlling process then send shoot, to notify
                    %% espdy_session that it now owns the socket.
                    ok = ssl:controlling_process(Sock, Pid),
                    Pid ! shoot,
                    accept_loop(ListenSock);
                X ->
                    %% Chrome seems to make non-ssl connections occasionally or something
                    io:format("ssl_accept: ~p\n",[X]),
                    accept_loop(ListenSock)
            end
    end.

