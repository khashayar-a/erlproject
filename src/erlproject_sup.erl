-module(erlproject_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("records.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?Log("erlproject_sup:init",[]),
    crypto:start(),
    emysql:start(),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,

    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    CUnit = {erlproject_cunit , {erlproject_cunit , start_link , []},
             Restart , Shutdown , Type, [erlproject_cunit]},
    DB = {erlproject_db , {erlproject_db , start_link , []},
          Restart , Shutdown , Type , [erlproject_db]},

    %% My_sql = {emysql , {emysql , start , []},
    %%           Restart , Shutdown , Type, [emysql]},

   {ok, {SupFlags, [DB,CUnit]}}.
