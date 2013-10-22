%%%-------------------------------------------------------------------
%%% @author Khashayar 
%%% @copyright (C) 2013, Khashayar
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Khashayar 
%%%-------------------------------------------------------------------
-module(erlproject_app).

-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
start(normal, _Args) ->
    error_logger:tty(false),
    error_logger:logfile({open, log_report}),
    erlproject_supervisor:start_link().

%%--------------------------------------------------------------------
stop(State) ->
    error_logger:info_report(["erlproject_supervisor stopped",{state, State}]),
    ok.

