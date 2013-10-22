%%%-------------------------------------------------------------------
%%% @author Khashayar 
%%% @copyright (C) 2013, Khashayar
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Khashayar
%%%-------------------------------------------------------------------
-module(erlproject_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,

    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    CUnit = {erlproject_cunit , {erlproject_cunit , start_link , []},
             Restart , Shutdown , worker, [erlproject_cunit]},
    DB = {erlproject_db , {erlproject_db , start_link , []},
          Restart , Shutdown , worker , [erlproject_db]},

    My_Sql = {mysql , {mysql , 
                       start_link , 
                       [p1, "127.0.0.1", 
                        3306,"evabihari", "ethebi1", "erlproject"
                       ]
		      },
	      Restart , Shutdown , worker, [mysql]},
    {ok, {SupFlags, [My_Sql,DB,CUnit]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
