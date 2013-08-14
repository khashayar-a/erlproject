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

-define(SERVER, ?MODULE). 


%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
init([]) ->
    State = erlproject_funs:source_gen(calendar:universal_time()),
    erlproject_parser:start(gen_url(hd(State))),
    io:format("Spawn ~p ~n",[gen_url(hd(State))]),
    {ok, tl(State)}.


%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
handle_cast({next, Url}, State) ->
    erlproject_parser:start(Url),
%    io:format("SPAWNing NExt ~p ~n",[Url]),
    {noreply, State};
handle_cast({language, Url}, State) ->
    erlproject_parser:start(Url),
%    io:format("SPAWNing LANG ~p ~n",[Url]),
    {noreply, State};
handle_cast({commit, Url}, State) ->
    erlproject_parser:start(Url),
%    io:format("SPAWNing Commit ~p ~n",[Url]),
    {noreply, State};
handle_cast(last, []) ->
    State = erlproject_funs:source_gen(calendar:universal_time()),
    io:format("Round Compelete ~n",[]),
    erlproject_parser:start(gen_url(hd(State))),
%    io:format("Spawn ~p ~n",[gen_url(hd(State))]),
    {noreply, tl(State)};
handle_cast(last, State) ->
%    io:format("SPAWN Last~n",[]),
    erlproject_parser:start(gen_url(hd(State))),
%    io:format("Spawn ~p ~n",[gen_url(hd(State))]),
    {noreply, tl(State)};
handle_cast({database, Reason}, State) ->
%    io:format("Data Error ~p~n",[Reason]),
    error_logger:info_report(["Database",{reason,Reason}]),
    {noreply, State};
handle_cast({wait, T, Url}, State) ->
    Now = epoch_now(),
    io:format("Waiting ~p Secs from ~p ~n",[T-Now , calendar:local_time()]),
    timer:sleep( max(T - Now,0) * 1000),
%    io:format("Re Spawning ~p ~n",[Url]),
    erlproject_parser:start(Url),
    {noreply, State};
handle_cast({error,Reason},[]) ->
    State = erlproject_funs:source_gen(calendar:universal_time()),
    error_logger:info_report(["Unknown Error",{reason,Reason}]),
    erlproject_parser:start(gen_url(hd(State))),
%    io:format("Error ~p~n",[Reason]),
    {noreply, tl(State)};
handle_cast({error,Reason},State) ->
%    io:format("Unknown Erorr ~p ~n",[Reason]),
    erlproject_parser:start(gen_url(hd(State))),
    error_logger:info_report(["Unknown Error",{reason,Reason}]),
    {noreply, tl(State)};
handle_cast(Msg,State) ->
    error_logger:info_report(["WTF",{message,Msg}]),
    {noreply, State}.



%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
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
gen_url(C) ->
    Auth = "&access_token=e62fdebb6e20c178dd30febcc7126e06367dd975",
    Page = "&page=1&per_page=100",
    Src = "https://api.github.com/search/repositories",
    Query = "?q=language:erlang+created:",
    {git,Src ++ Query ++ C ++ Page ++ Auth}.


epoch_now() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time())
	-719528*24*3600.

