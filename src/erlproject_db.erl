%%%-------------------------------------------------------------------
%%% @author Khashayar 
%%% @copyright (C) 2013, Khashayar
%%% @doc
%%%     Grabbing Data via message from the parser
%%%     Generating the proper sql query 
%%%     Executing the query via mysql server
%%% @end
%%% Created : 26 Jul 2013 by Khashayar 
%%%-------------------------------------------------------------------
-module(erlproject_db).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, gen_query/2, fix/1, fix/2]).
-export([fix_comment/1]).

-define(SERVER, ?MODULE). 

-include("records.hrl").

-define(DATE_LENGTH, 10).
-define(TIME_LENGTH, 8).


%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
init([]) ->
    connect(),
    {ok, []}.

%%--------------------------------------------------------------------
handle_call({get_value,{git, ReturnField, FilterField, Value}}, _From, State) ->
    SqlReq = gen_query(get_value,{git, ReturnField, FilterField, Value}), 
    Result = case query_function(get, SqlReq) of
                 {error,_} ->
                     {novalue,undefined};
                 {ok,[[undefined]|_]} -> {novalue, undefined};
                 {ok,[]} ->  {novalue, undefined};
                 {ok, [[]|_]} -> {novalue, undefined};
                 {ok, [[BinValue]|_]} ->  
                     %% example: [[<<"2013-10-14T11:55:36Z">>]]
                     {value, binary_to_list(BinValue)}
             end,
    {reply, Result, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({write, git, Data = #git{}}, State) ->
    fix_clarity(Data), 
    case gen_query(git, Data) of
        {Query_Git, Query_Owner} ->

            Res1 = query_function(write, Query_Git),
            Res2 = query_function(write, Query_Owner),
            case {Res1,Res2} of
                {ok,ok} ->
                    {noreply, State};
                {ok,{error,R}} ->
                    gen_server:cast(erlproject_cunit,{database,R}),
                    {noreply, State};
                {{error,R},ok} ->
                    gen_server:cast(erlproject_cunit,{database,R}),
                    {noreply, State};
                {{error,R1},{error,R2}} ->
                    gen_server:cast(erlproject_cunit,{database,{R1,R2}}),
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;
handle_cast({write, git_language, Data}, State) ->
    Query = gen_query(git_language, Data),
    Res = query_function(write,Query),
    case Res of
	ok ->
	    {noreply, State};
	{error,Reason} ->
	    gen_server:cast(erlproject_cunit,{database,Reason}),
	    {noreply, State}
    end;

handle_cast({write, git_commit, Data}, State) ->
    Query = gen_query(git_commit, Data),
    Res = query_function(write,Query),
    case Res of
	ok ->
	    {noreply, State};
	{error,Reason} ->
	    gen_server:cast(erlproject_cunit,{database,Reason}),
	    {noreply, State}
    end;

handle_cast({write, google, Data = #git{}}, State) ->
    Query = gen_query(google, Data),
    Res = query_function(write,Query),
    case Res of
	ok ->
	    {noreply, State};
	{error,Reason} ->
	    gen_server:cast(erlproject_cunit,{database,Reason}),
	    {noreply, State}
    end;
handle_cast({write, sfapi, Data = #git{}}, State) ->
    Query = gen_query(sfapi, Data),
    Res = query_function(write,Query),
    case Res of
	ok ->
	    {noreply, State};
	{error,Reason} ->
	    gen_server:cast(erlproject_cunit,{database,Reason}),
	    {noreply, State}
    end;
handle_cast({write, bbapi, Data = #git{}}, State) ->
    Query = gen_query(bbapi, Data),
    Res = query_function(write,Query),
    case Res of
	ok ->
	    {noreply, State};
	{error,Reason} ->
	    gen_server:cast(erlproject_cunit,{database,Reason}),
	    {noreply, State}
    end;
handle_cast(Msg, State) ->
    io:format("erlproject_db Message coming ~p~n" , [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    ?L("erlproject_db terminate",[{reason,Reason}]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect() ->
                                                %    mysql:start(p1, "db.student.chalmers.se",
                                                %		3306, "abdoli", "kgcH8v7c", "abdoli").
    mysql:start(p1, "127.0.0.1",
		3306, "evabihari", "ethebi1", "erlproject").

query_function(write, Q) ->
    try mysql:fetch(p1, Q) of
	Result ->
            case Result of
                {R,{_,_,_,_,_,_,_,_}} -> 

                    case R of
                        updated ->
                            ok;
                        error ->
                            {error, {sql_syntax,Q}};
                        _ ->
                            {error, R}
                    end;
                {error,R2} ->
                    {error,R2}
	    end
    catch
	exit:_Exit ->
	    {error, no_connection}
    end;
query_function(get, Q) ->
    try 
        case mysql:fetch(p1, Q) of
            {data,MySqlRes} ->
                {ok, mysql:get_result_rows(MySqlRes)};
            R -> {error, R}
        end
    catch
	exit:_Exit ->
	    {error, no_connection}
    end;                    
query_function(select,Q) ->
   try mysql:fetch(p1, Q) of
	Result ->
            case Result of
                {R,{_,_,Res,_,_,_,_,_}} -> 
                    case R of
                        data ->
                            {ok, Res};
                        error ->
                            {error, {sql_syntax,Q}};
                        _ ->
                            {error, R}
                    end;
                {error,R2} ->
                    {error,R2}
	    end
    catch
	exit:_Exit ->
	    {error, no_connection}
    end;

query_function(delete,Q) ->
   try mysql:fetch(p1, Q) of
	Result ->
            case Result of
                {R,{_,_,Re_,_,_,_,_,_}} -> 
                    case R of
                        updated ->
                            ok;
                        error ->
                            {error, {sql_syntax,Q}};
                        _ ->
                            {error, R}
                    end;
                {error,R2} ->
                    {error,R2}
	    end
    catch
	exit:_Exit ->
	    {error, no_connection}
    end.

gen_query(git, Data = #git{}) ->
    try
        {"insert into erlproject_git " ++
             " (id, name, full_name, owner_id, html_url, description," ++ 
             " created_at, updated_at, pushed_at , clone_url," ++
             " stars, open_issues, forks, source) values ('" ++
             integer_to_list(Data#git.id) ++ "' , '" ++
             Data#git.name  ++ "' , '" ++
             Data#git.full_name  ++ "' , '" ++
             integer_to_list(Data#git.owner#owner.id)  ++ "' , '" ++
             Data#git.html_url  ++ "' , '" ++
             fix(Data#git.description)  ++ "' , '" ++
                                                %        Data#git.languages  ++ "' , '" ++
             Data#git.created_at  ++ "' , '" ++ 
             Data#git.updated_at  ++ "' , '" ++
             Data#git.pushed_at  ++ "' , '" ++     
             Data#git.clone_url ++ "' , '" ++
             integer_to_list(Data#git.watchers)  ++ "' , '" ++
             integer_to_list(Data#git.open_issues)  ++ "' , '" ++
             integer_to_list(Data#git.forks)  ++ 
             "' , 'github') on duplicate key update " ++
             " forks = '" ++ integer_to_list(Data#git.forks) ++
             "' , stars = '" ++ integer_to_list(Data#git.watchers) ++
             "' , open_issues = '" ++ integer_to_list(Data#git.open_issues) ++
             "' , pushed_at = '" ++ Data#git.pushed_at ++
             "' , updated_at = '" ++ Data#git.updated_at ++ 
             "' , clone_url = '" ++ Data#git.clone_url ++ "'",

         "insert into erlproject_owner " ++
             " (id, login, avatar_url, url) values (' " ++
             integer_to_list(Data#git.owner#owner.id)  ++ "' , '" ++
             Data#git.owner#owner.login  ++ "' , '" ++
             Data#git.owner#owner.avatar_url  ++ "' , '" ++
             Data#git.owner#owner.url  ++ "') " ++
             " on duplicate key update url = ' " ++
             Data#git.owner#owner.url  ++ "' "}
    of
        {Query_Git, Query_Owner} -> {Query_Git, Query_Owner}
    catch
        error:_Reason ->
            error
    end;

gen_query(git_language, {Body,Name}) ->
    "update erlproject_git " ++
	"set languages = '" ++ 
	fix(Body) ++ "' " ++
	"where full_name = '" ++
	Name ++ "'";

gen_query(git_commit, {Number,Data}) ->
    Url =  Data#commit.url,
    L = string:tokens(Url, "/"),
    Name = lists:nth(3,L) ++ "/" ++ lists:nth(4,L),
    "insert into erlproject_commits" ++
	" (number, id , sha , author, date, url , message) " ++
	" values ('" ++
	integer_to_list(Number) ++ "' , " ++
	"(select id from erlproject_git where full_name = '" ++
	Name ++ "') , '" ++
	Data#commit.sha ++ "' , '" ++
	fix(Data#commit.author) ++ "' , '" ++
	Data#commit.date ++ "' , '" ++
	Data#commit.url ++ "' , '" ++
	fix_comment(fix(Data#commit.message)) ++ "') on duplicate key update " ++ 
	"sha = '" ++ Data#commit.sha ++ 
	"' , author = '" ++ fix(Data#commit.author) ++ 
	"' , date = '" ++ Data#commit.date ++ 
	"' , url = '" ++ Data#commit.url ++ 
	"' , message = '" ++ fix_comment(fix(Data#commit.message)) ++ "'";

gen_query(git_commit_delete, Name) ->
    "delete from erlproject_commits where id = " ++
	"(select id from erlproject_git where full_name = '"++
	Name ++ "')";

gen_query(git_project_delete, Id) ->
    "delete from erlproject_git where id = '" ++ integer_to_list(Id) ++ "'"; 


gen_query(git_clarity, Data= #git{}) ->
    Url =  Data#git.html_url,
    L = string:tokens(Url, "/"),
    Name = lists:nth(3,L) ++ "/" ++ lists:nth(4,L),
    "select id from erlproject_git where full_name = '" ++
	Name ++ "'";   

gen_query(google, Data = #git{}) ->
    "insert into erlproject_git " ++
	" (id, name, full_name, html_url, description," ++ 
	" languages, updated_at," ++
	" stars, source) values ('" ++
	integer_to_list(Data#git.id) ++ "' , '" ++
        Data#git.name  ++ "' , '" ++
        fix(Data#git.full_name)  ++ "' , '" ++
        Data#git.html_url  ++ "' , '" ++
        fix(Data#git.description)  ++ "' , '" ++
        Data#git.languages  ++ "' , '" ++
        fix(Data#git.updated_at)  ++ "' , '" ++
	integer_to_list(Data#git.watchers)  ++ "'," ++
	"'google') on duplicate key update " ++
	" stars = '" ++ integer_to_list(Data#git.watchers) ++
	"', updated_at = if(updated_at < '" ++
        fix(Data#git.updated_at) ++ "', '" ++
        fix(Data#git.updated_at) ++ "', updated_at)";
gen_query(sfapi, Data = #git{}) ->
    "insert into erlproject_git " ++
	" (id, name, full_name, html_url, description," ++ 
	" languages, created_at," ++
	" stars, source) values ('" ++
	integer_to_list(Data#git.id) ++ "' , '" ++
        Data#git.name  ++ "' , '" ++
        fix(Data#git.full_name)  ++ "' , '" ++
        Data#git.html_url  ++ "' , '" ++
        fix(Data#git.description)  ++ "' , '" ++
        fix(Data#git.languages)  ++ "' , '" ++
        fix(Data#git.created_at)  ++ "' , '" ++
	integer_to_list(Data#git.watchers)  ++ "'," ++
	"'sourceforge') on duplicate key update " ++
	" stars = stars";    
gen_query(bbapi, Data = #git{}) ->
    "insert into erlproject_git " ++
	" (id, name, full_name, html_url, description," ++ 
	" languages, created_at, updated_at," ++
	" stars, forks, source) values ('" ++
	integer_to_list(Data#git.id) ++ "' , '" ++
        Data#git.name  ++ "' , '" ++
        fix(Data#git.full_name)  ++ "' , '" ++
        Data#git.html_url  ++ "' , '" ++
        fix(Data#git.description)  ++ "' , '" ++
        fix(Data#git.languages)  ++ "' , '" ++
        fix(bbapi_date,Data#git.created_at)  ++ "' , '" ++
        fix(bbapi_date,Data#git.updated_at)  ++ "' , '" ++
	integer_to_list(Data#git.watchers)  ++ "', '" ++
	integer_to_list(Data#git.forks)  ++ "'," ++
	"'bitbucket') on duplicate key update " ++
	" forks = '" ++ integer_to_list(Data#git.forks) ++
	"' , stars = '" ++ integer_to_list(Data#git.watchers) ++
	"' , updated_at = '" ++ fix(bbapi_date,Data#git.updated_at) ++ "'";
gen_query(get_value,{git, ReturnField, Field, Value}) when is_integer(Value) ->
    gen_query(get_value,{git, ReturnField, Field, integer_to_list(Value)});
gen_query(get_value,{git, ReturnField, Field, Value}) ->
    "SELECT "++ ReturnField ++ " from erlproject_git where "++Field++" = '" ++ Value++"'".

fix_comment(String) ->
    S1 = re:replace(String,"'","", [global,{return,list}]),
    A=re:replace(S1,"\\\\","", [global,{return,list}]),
    re:replace(re:replace(A, "\\s+$", "", [global,{return,list}]), "^\\s+", "", [global,{return,list}]).

fix({array,List})->
    fix(language,List,[]);
fix({{YY,MM,DD},{HH,Mm,SS}}) ->
    integer_to_list(YY) ++ "-" ++
        integer_to_list2(MM) ++ "-" ++
	integer_to_list2(DD) ++ "T" ++
	integer_to_list2(HH) ++ ":" ++
	integer_to_list2(Mm) ++ ":" ++
	integer_to_list2(SS)++"Z";
fix(null) ->
    [];
fix(undefined) ->
    [];
fix(D) ->
    fix(D,[]).

fix(language,[H],Buff) ->
    Buff ++ H;
fix(language,[H|T],Buff) ->
    fix(language,T,Buff ++ H ++ ",").

fix(bbapi_date, Date) ->
    string:left(Date,?DATE_LENGTH) ++ "T" ++ 
        string:substr(Date,?DATE_LENGTH+2,?TIME_LENGTH) ++"Z";
fix([], Buff) ->
    Buff;

fix([39|T], Buff) ->
    fix(T, Buff ++ [92, 39]);

fix([38,35,51,57,59|T], Buff) ->
    fix(T, Buff ++ [92, 39]);

fix([34|T], Buff) ->
    fix(T, Buff ++ [92, 34]);

fix([H|T], Buff) when is_list(H)->
    fix(T, Buff ++ fix(H,[]));

fix([H|T], Buff) when H < 127->
    fix(T, Buff ++ [H]);
fix([_|T], Buff) ->
    fix(T, Buff).


fix_clarity(Data= #git{}) ->
    %% check that id in the git project is individual
    %% there can be problems with github to return different id?
    %% Url =  Data#git.html_url,
    %% L = string:tokens(Url, "/"),
    %% Name = lists:nth(3,L) ++ "/" ++ lists:nth(4,L),
    Q = gen_query(git_clarity, Data),
    case query_function(select,Q) of
        {ok,[]} ->
            %% ?Log("fix_clarity",{url,Data#git.html_url}),
            ok;
        {ok,List} ->
            %% ?Log("fix_clarity",{ids,List}),

            case length(lists:flatten(List)) of 
                1 -> ok;
                _N ->
                    % more than 1 git project with the same name - let's  them
                    error_logger:info_report(["More than 1 git_project",
                                              {url,Data#git.html_url},{ids,List}]),
                    remove_projects(lists:flatten(List))
            end
    end.
    
remove_projects([]) ->
    ok;
remove_projects([L|H]) ->
    Q = gen_query(git_project_delete, L),
    query_function(delete,Q),
    remove_projects(H).

integer_to_list2(X) when X<10 ->
    integer_to_list(0) ++ integer_to_list(X);
integer_to_list2(X) ->
    integer_to_list(X).

                             
    
