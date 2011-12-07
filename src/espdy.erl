-module(espdy).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

start() -> application:start(espdy).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    espdy_sup:start_link().

stop(_State) ->
    ok.
