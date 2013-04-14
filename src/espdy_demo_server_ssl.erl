-module(espdy_demo_server_ssl).
-export([start/1]).

%% This is not a scalable way to accept tcp connections - it's just a demo
%% Do not benchmark the server using this code:
start(Port) when is_integer(Port) ->
    ok = ssl:start(),
    spawn_link(fun() ->
        {ok, Listen} = ssl:listen(Port, [
            {next_protocols_advertised, [<<"spdy/3">>, <<"spdy/2">>, <<"http/1.0">>, <<"http/1.1">>]},
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
                    case ssl:negotiated_next_protocol(Sock) of
                        {ok, Proto} ->
                            io:format("Negotiated next protocol: ~p\n",[Proto]),
                            SpdyVersion = spdy_version_from_proto(Proto),
                            Opts = [{spdy_version, SpdyVersion}], %% passed through to the stream callback module
                            {ok, Pid} = espdy_session:start_link(Sock, ssl, espdy_stream_http, Opts),
                            %% You must assign controlling process then send shoot, to notify
                            %% espdy_session that it now owns the socket.
                            ok = ssl:controlling_process(Sock, Pid),
                            Pid ! shoot,
                            accept_loop(ListenSock);
                        {error,next_protocol_not_negotiated} ->
                            io:format("failed next protocol negotiation~n"),
                            ok = ssl:close(Sock)
                    end;
                X ->
                    %% Chrome seems to make non-ssl connections occasionally or something
                    io:format("ssl_accept: ~p\n",[X]),
                    accept_loop(ListenSock)
            end
    end.

spdy_version_from_proto(Proto) ->
    case binary_to_list(Proto) of
        "spdy/3" -> 3;
        _ -> 2
    end.

