%%%-------------------------------------------------------------------
%%% @author Khashayar
%%% @copyright (C) 2013, Khashayar
%%% @doc
%%%     This process is the core of the program
%%%     It controlls the workflow and manages the process that need to start
%%% @end
%%% Created : 25 Jul 2013 by Khashayar
%%%-------------------------------------------------------------------
-module(erlproject_cunit).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([epoch_now/0]).
                                                % just for test
-export([gen_url/1,  check_other_parser/0, kill_other_parsers/0]).

-include("records.hrl").

-define(SERVER, ?MODULE). 

%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%-------------------------------------------------------------------
init([]) ->
    State = erlproject_funs:source_gen(calendar:universal_time()),
    erlproject_parser:start(gen_url(hd(State))),
    ?L("Spawn",[{url,gen_url(hd(State))}]),
    {ok, tl(State)}.


%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
handle_cast({next, Url}, State) ->
    erlproject_parser:start(Url),
    {noreply, State};
%% git parsing redesigned to a sequential issue
%% handle_cast({language, Url}, State) ->
%%     erlproject_parser:start(Url),
%%     {noreply, State};
%% git parsing redesigned to a sequential issue
%% handle_cast({commit, Url}, State) ->
%%     erlproject_parser:start(Url),
%%     {noreply, State};
handle_cast(last, []) ->
    State = erlproject_funs:source_gen(calendar:universal_time()),
    error_logger:info_report(["Round Compelete",{time,calendar:local_time()}]),
    %% kill erlproject_cunit in order to destroy parser processes (if there are more than 1)
    N = check_other_parser(),
    if (N>0) ->    kill_other_parsers();
       true -> ok
    end,
    erlproject_parser:start(gen_url(hd(State))),
    {noreply, tl(State)};
handle_cast(last, State) ->
    %% error_logger:info_report(["last received",{reason,hd(State)}]),
    erlproject_parser:start(gen_url(hd(State))),
    {noreply, tl(State)};
handle_cast({database, Reason}, State) ->
    error_logger:info_report(["Database",{reason,Reason}]),
    {noreply, State};
handle_cast({wait, T, Url}, State) ->
    Now = epoch_now(),
    ?Log("Waiting",[{sec, T-Now},{time,calendar:local_time()}]),
    if T>Now ->
            timer:sleep( max(T - Now,1) * 1000);
       true -> ok
    end,
    case check_other_parser() of
        0 -> %% new process to be started if ther is no other parsing process exist
            erlproject_parser:start(Url);
        _ -> ok
    end,
    {noreply, State}; 

handle_cast({error,Reason},[]) ->
    State = erlproject_funs:source_gen(calendar:universal_time()),
    error_logger:info_report(["Unknown Error",{reason,Reason}]),
    case check_other_parser() of
        N when (N <2)  -> %% new process to be started if ther is no other parsing process exist
            erlproject_parser:start(gen_url(hd(State)));

        _ -> ok
    end,    {noreply, tl(State)};

handle_cast({error,Reason},State) ->
    error_logger:info_report(["Unknown Error with State",{reason,Reason},{state,hd(State)}]),
    case check_other_parser() of
        N when (N <2)  -> %% new process to be started if ther is no other parsing process exist
            erlproject_parser:start(gen_url(hd(State)));
        _ -> ok
    end,
    {noreply, tl(State)};

handle_cast(Msg,State) ->
    error_logger:info_report(["WTF",{message,Msg}]),
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:info_report(["erlproject_cunit Handle_info",{reason,Info}]),
    {noreply, State}.

terminate(normal, _State) ->
    error_logger:info_report(["erlproject_cunit terminated normally"]),
    ok;
terminate(Reason, _State) ->
    error_logger:info_report(["erlproject_cunit terminated",{reason,Reason}]),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions
%%--------------------------------------------------------------------
gen_url(sourceforge) ->
    {sourceforge, 
     "http://sourceforge.net/directory/language:erlang/?q="};
gen_url(bitbucket) ->
    {bitbucket, 
     "https://bitbucket.org/repo/all/relevance?name=erlang&language=erlang"};
gen_url(google) ->
    {google,
     "https://code.google.com/hosting/search?"++
	 "q=label%3Aerlang&filter=0&mode=&start=0"};
gen_url({l,C}) ->
    Auth = "&access_token=e62fdebb6e20c178dd30febcc7126e06367dd975",
    Page = "&page=1&per_page=100", 
    Src = "https://api.github.com/search/repositories",
    Query = "?q=language:erlang+created:",
    {git,Src ++ Query ++ C ++ Page ++ Auth};
gen_url({s,C}) ->
    Auth = "&access_token=e62fdebb6e20c178dd30febcc7126e06367dd975",
    Page = "&page=1&per_page=100",
    Src = "https://api.github.com/search/repositories",
    Query = "?q=erlang+created:",
    {git,Src ++ Query ++ C ++ Page ++ Auth}.

epoch_now() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time())
	-719528*24*3600.

check_other_parser() ->
    %% return number of erlproject_parser (or erlproject_git_parser) processes running
    Pids = erlang:processes(),
    check_process_info(Pids).

check_process_info(Pids) ->
    check_process_info(Pids,0).

check_process_info([],N) ->
    N;
check_process_info([Pid|Pids],N) ->
    case  erlang:process_info(Pid,initial_call) of
        {initial_call,{erlproject_parser,_F,_Pn}} -> check_process_info(Pids,N+1);
        {initial_call,{erlproject_git_parser,_F,_Pn}} -> check_process_info(Pids,N+1);
        _ -> check_process_info(Pids,N)
    end.

kill_other_parsers() ->
    %% wait a bit to finish parallel processes
    timer:sleep(11000),
    Pids = erlang:processes(),
    kill_other_parsers(Pids).

kill_other_parsers([]) ->
    ok;
kill_other_parsers([Pid|Pids]) ->
    case  erlang:process_info(Pid,initial_call) of
        {initial_call,{erlproject_parser,_F,_Pn}} -> 
            ?L("kill erlproject_parser",[]),
            exit(Pid,kill),
            kill_other_parsers(Pids);
        {initial_call,{erlproject_git_parser,_F,_Pn}} -> 
            ?L("kill erlproject_git_parser",[]),
            exit(Pid,kill),
            kill_other_parsers(Pids);
        _ -> kill_other_parsers(Pids)
    end.    

