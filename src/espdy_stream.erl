%%
%% One of these is spawned per stream (SYN_STREAM)
%%
%% It delegates to the (currently hardcoded) callback module, which will
%% makes the request look like a normal http req to the app
%% 
-module(espdy_stream).
-behaviour(gen_server).

-include("include/espdy.hrl").

-compile(export_all).

%% API
-export([start_link/5, send_data_fin/1, send_data/2, closed/2, received_data/2,
         send_response/3, received_fin/1, send_frame/2, headers_updated/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {streamid,
                clientclosed = false, %% them
                serverclosed = false, %% us
                pid,
                mod,
                mod_state,
                headers,
                spdy_version,
                spdy_opts}).

%% API
start_link(StreamID, Pid, Headers, Mod, Opts) ->
    gen_server:start(?MODULE, [StreamID, Pid, Headers, Mod, Opts], []).

send_data(Pid, Data) when is_pid(Pid), is_binary(Data) ->
    gen_server:cast(Pid, {data, Data}).

send_data_fin(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {data, fin}).

closed(Pid, Reason) when is_pid(Pid) ->
    gen_server:cast(Pid, {closed, Reason}).

received_data(Pid, Data) ->
    gen_server:cast(Pid, {received_data, Data}).

received_fin(Pid) ->
    gen_server:cast(Pid, received_fin).

send_response(Pid, Headers, Body) ->
    gen_server:cast(Pid, {send_response, Headers, Body}).

send_frame(Pid, F) ->
    gen_server:cast(Pid, {send_frame, F}).

headers_updated(Pid, Delta, NewMergedHeaders) ->
    gen_server:cast(Pid, {headers_updated, Delta, NewMergedHeaders}).

%% gen_server callbacks

init([StreamID, Pid, Headers, Mod, Opts]) ->
    gen_server:cast(self(), init_callback),
%%    Z = zlib:open(),
%%    ok = zlib:deflateInit(Z),
    %%ok = zlib:deflateInit(Z, best_compression,deflated, 15, 9, default),
%%    zlib:deflateSetDictionary(Z, ?HEADERS_ZLIB_DICT),
    SpdyVersion = proplists:get_value(spdy_version, Opts),
    {ok, #state{streamid=StreamID,
                pid=Pid,
                mod=Mod,
                headers=Headers,
                spdy_version=SpdyVersion,
                spdy_opts=Opts}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% Called when we got a syn_stream for this stream.
%% cb module is supposed to make and send the reply.
handle_cast(init_callback, State) ->
    case (State#state.mod):init(self(), State#state.headers, State#state.spdy_opts) of
        %% In this case, the callback module provides the full response
        %% with no need for this process to persist for streaming the body
        %% so we can terminate this process after replying.
        {ok, Headers, Body} when is_list(Headers), is_binary(Body) ->
            %% se re-send this as a message to ourselves, because the callback
            %% module may have dispatched other frames (eg, settings) before 
            %% returning us this response:
            send_response(self(), Headers, Body),
            {noreply, State};
        %% The callback module will call msg us the send_http_response
        %% (typically from within the guts of cowboy_http_req, so that
        %%  we can reuse the existing http API)
        {ok, noreply} ->
            %% TODO track state, set timeout on getting the response from CB
            {noreply, State};
        {ok, noreply, ModState} ->
            {noreply, State#state{mod_state=ModState}};
        %% CB module is going to stream us the body data, so we keep this process
        %% alive until we get the fin packet as part of the stream.
   %%%% {ok, Headers, stream, ModState} when is_list(Headers) ->
   %%%%     NVPairsData = encode_name_value_pairs(Headers, State#state.z_context),
   %%%%     StreamID = State#state.streamid,
   %%%%     F = #cframe{type=?SYN_REPLY,
   %%%%                 flags=0, 
   %%%%                 length = 6 + byte_size(NVPairsData),
   %%%%                 data= <<0:1, 
   %%%%                         StreamID:31/big-unsigned-integer, 
   %%%%                         0:16/big-unsigned-integer, %% UNUSED
   %%%%                         NVPairsData/binary
   %%%%                       >>},
   %%%%     espdy_session:snd(State#state.pid, StreamID, F),
   %%%%     {noreply, State#state{mod_state=ModState}};
        %% CB module can't respond, because request is invalid
        {error, not_http} ->
            StreamID = State#state.streamid,
            F = #spdy_rst_stream{ streamid=StreamID, statuscode=?PROTOCOL_ERROR },
            espdy_session:snd(State#state.pid, StreamID, F),
            {stop, normal, State}
    end;

handle_cast({send_frame, F}, State) ->
    StreamId = State#state.streamid,
    FrameType = element(1, F),
    F2 = case FrameType of
        spdy_data ->
            setelement(2, F, StreamId);
        spdy_window_update ->
            setelement(3, F, StreamId);
        _ ->
            setelement(4, F, StreamId)
    end,
    FullF = case FrameType of
        spdy_data -> F;
        _ -> setelement(2, F2, State#state.spdy_version)
    end,
    espdy_session:snd(State#state.pid, StreamId, FullF),
    {noreply, State};

handle_cast(received_fin, State = #state{clientclosed=true}) ->
    ?LOG("Got FIN but client has already closed?", []),
    {stop, {stream_error, protocol_error}, State};

handle_cast(received_fin, State = #state{clientclosed=false}) ->
    NewState = State#state{clientclosed=true},
    case both_closed(NewState) of
        true  -> ?LOG("Both ends closed, stopping stream ~w",[State#state.streamid]),
                 {stop, normal, NewState};
        false -> ?LOG("Client closed, server not. stream ~w",[State#state.streamid]),
                 {noreply, NewState}
    end;

handle_cast({received_data, Data}, State) ->
    {ok, NewModState} = (State#state.mod):handle_data(Data, State#state.mod_state),
    {noreply, State#state{mod_state=NewModState}};

handle_cast({headers_updated, Delta, NewMergedHeaders}, State) ->
    {ok, NewModState} = (State#state.mod):headers_updated(Delta, NewMergedHeaders, State#state.mod_state),
    {noreply, State#state{mod_state=NewModState}};

handle_cast({closed, Reason}, State) ->
    (State#state.mod):closed(Reason, State#state.mod_state),
    {stop, normal, State};

%% part of streamed body
handle_cast({data, Bin}, State) when is_binary(Bin) ->
    F = #spdy_data{ streamid = State#state.streamid,
                    data=Bin},
    espdy_session:snd(State#state.pid, State#state.streamid, F),
    {noreply, State};

%% last of streamed body
handle_cast({data, fin}, State) ->
    F = #spdy_data{ streamid = State#state.streamid,
                    flags=?DATA_FLAG_FIN,
                    data= <<"">>},
    espdy_session:snd(State#state.pid, State#state.streamid, F),
    NewState = State#state{serverclosed=true},
    case both_closed(NewState) of
        true  -> ?LOG("Both ends closed, stopping stream ~w",[State#state.streamid]),
                 {stop, normal, NewState};
        false -> ?LOG("We are closed, client not. stream ~w",[State#state.streamid]),
                 {noreply, NewState}
    end;

handle_cast({send_response, Headers, Body}, State) ->
    send_http_response(Headers, Body, State),
    NewState = State#state{serverclosed=true},
    case both_closed(NewState) of
        true  -> ?LOG("Both ends closed, stopping stream ~w",[State#state.streamid]),
                 {stop, normal, NewState};
        false -> ?LOG("We are closed, client not. stream ~w",[State#state.streamid]),
                 {noreply, NewState}
    end.
    
handle_info(M, State) ->
    {noreply, NewModState} = (State#state.mod):handle_info(M, State#state.mod_state),
    {noreply, State#state{mod_state=NewModState}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

send_http_response(Headers, Body, State = #state{}) when is_list(Headers), is_binary(Body) ->
    io:format("Respond with: ~p ~p\n",[Headers, Body]),
    StreamID = State#state.streamid,
    F = #spdy_syn_reply{ version = State#state.spdy_version,
                         streamid = StreamID,
                         headers = Headers
                       },
    espdy_session:snd(State#state.pid, StreamID, F),
    %% Send body response in exactly one data frame, with fin set.
    %% TODO fragment this if we hit some maximum frame size?
    F2 = #spdy_data{streamid=StreamID, 
                    flags=?DATA_FLAG_FIN,
                    data=Body},
    espdy_session:snd(State#state.pid, StreamID, F2),
    ok.



both_closed(#state{clientclosed=true,serverclosed=true}) -> true;
both_closed(_) -> false.
