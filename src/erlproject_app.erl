-module(erlproject_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:tty(false),
    error_logger:logfile({open, log_report}),
    lager:start(),
    erlproject_sup:start_link().

stop(State) ->
    error_logger:info_report(
      ["erlproject_app stopped",{state, State}]),
    ok.
