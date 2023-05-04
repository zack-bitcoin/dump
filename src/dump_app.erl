-module(dump_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, mode/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

mode() ->
    hd.


start(_StartType, _StartArgs) ->
    %Amount = 1000000,
    Amount = 1000,
    Type = mode(),
    dump_sup:start_link(dump, 32, Amount, Type, "").

stop(_State) ->
    ok.
