-module(espdy_demo_server).
-export([start/2]).

%% This is not a scalable way to accept tcp connections - it's just a demo
%% Do not benchmark the server using this code:
start(Port, _Ssl = false) when is_integer(Port) ->
    spawn_link(fun() ->
        {ok, Sock} = gen_tcp:listen(Port, [{active,false}, {packet, raw}, binary, {reuseaddr, true}]),
        io:format("Spdy demo server listening on http://localhost:~B/\n",[Port]),
        accept_loop(Sock)
    end).

accept_loop(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Sock} ->
            Opts = [], %% passed through to the stream callback module
            {ok, Pid} = espdy_session:start_link(Sock, gen_tcp, Opts),
            %% You must assign controlling process then send shoot, to notify
            %% espdy_session that it now owns the socket.
            ok = gen_tcp:controlling_process(Sock, Pid),
            Pid ! shoot,
            accept_loop(ListenSock);
        {error, Err} -> 
            error_logger:error_msg("Failed to accept connection: ~p",[Err]),
            fail
    end.


