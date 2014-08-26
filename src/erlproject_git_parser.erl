%%%-------------------------------------------------------------------
%%% @author evabihari <eva.bihari@erlang-solutions.com>
%%% @copyright (C) 2013, eva.bihari
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2013 by eva.bihari <eva.bihari@erlang-solutions.coml>
%%%-------------------------------------------------------------------
-module(erlproject_git_parser).

-export([start/1, init/1]).

-export([create_date/1]).
-include("records.hrl").

start(Url) ->
    spawn(erlproject_git_parser, init, [{self(), Url}]).

init({From, Url}) ->
    crawl(From,{git,Url}).

crawl(From,{git, Url}) ->
    %% error_logger:info_report(["crawl git", {url, Url}]),

    case erlproject_funs:read_web(git,Url) of
	{limit, T} ->
            wait(T),
            crawl(From,{git, Url});
	{success, last, List} ->
            ParsedList = parse(git,List),
            getInformation(From, ParsedList),
    	    gen_server:cast(erlproject_cunit, last);
	{success, Next, List} ->
            ParsedList = parse(git,List),
            getInformation(From, ParsedList),
 	    gen_server:cast(erlproject_cunit, {next, {git,Next}});           
	{error,Reason} ->
            error_logger:info_report("erlproject_git_parser error received",
                                     [{error,Reason}]),
	    gen_server:cast(erlproject_cunit, {error, {Reason,Url}});
	_ ->
            error_logger:info_report(
              "erlproject_git_parser SOMETHING IS HAPPENING",[{url,Url}])
    end.

parse(git, List) ->
    Auth = "?access_token=" ++ ?GITHUB_ACCESS_TOKEN,
    Extract = fun(X) -> 
		      erlproject_funs:extract(git,X) 
	      end, 
    Res = lists:map(Extract , List),
    Cast = fun(X) -> 
                   R = case gen_server:call(erlproject_db,
                                            {get_value,
                                             {git,"updated_at",
                                              "id", X#git.id}}) of
                           {value, Last_updated} -> 
                               update_needed(X#git.updated_at,Last_updated);
                           %% updated_at filed in the stored data not 
                           %% smaller than the actual
                           _Other -> 
                               true  %% update needed
                       end,
                   case R of
                       true ->
                           Languages = X#git.languages_url ++ Auth,
                           Commits = hd(string:tokens(X#git.commits_url, "{")) 
                               ++ Auth ++ "&per_page=3",
                           gen_server:cast(erlproject_db,{write, git,  X}),
                           {Languages, Commits};
                       _ -> {[],[]}
                   end
           end,
    lists:map(Cast, Res).

getInformation(_From, []) ->
    ok;
getInformation(From,[{[],_}|Tail]) ->
    %% skip language and commit data as no update has happened 
    %%after our last update
    getInformation(From,Tail);
getInformation(From,[{Languages,Commits}|Tail]) ->
    case read_language(Languages) of
        success -> case read_commits(Commits) of
                       ok -> ok,
                             getInformation(From,Tail);
                       error -> skip_commits,
                                getInformation(From,Tail);
                       Limit -> wait(Limit),
                                getInformation(From,[{Languages,Commits}|Tail])
                   end;
        error -> skip_language,
                     getInformation(From,Tail);
        Limit -> wait(Limit),
                 getInformation(From,[{Languages,Commits}|Tail])
    end.


read_language(Languages) ->
    case erlproject_funs:read_web(default,Languages) of
	{success,{Headers,Body}} ->
	    case erlproject_funs:check(Headers) of 
		ok ->
		    LL = string:tokens(Languages, "/"),
		    Name = lists:nth(4,LL) ++ "/" ++ lists:nth(5,LL),
		    gen_server:cast(erlproject_db,
				    {write, git_language, {Body,Name}}),
                    success;
                error ->
                    error;
                Limit ->
                    Limit
            end;
        {error, Reason} ->
            %% reading error to be logged
            error_logger:info_report(["read_language failed",{reason,Reason}]),
            error
    end.

read_commits(Commits) ->
    case erlproject_funs:read_web(default,Commits) of
	{success,{Headers,Body}} ->
	    case erlproject_funs:check(Headers) of 
		ok ->
		    {array,List} = mochijson:decode(Body),
                    %% store the commits to the DB
		    erlproject_parser:parse(git_commit, List);
                error ->
                    error;
                Limit ->
                    Limit
            end;
        {error, Reason} ->
            %% reading error to be logged
            error_logger:info_report(["read_commits failed",{reason,Reason}]),
            error
    end.

wait(T) ->
    Now = erlproject_cunit:epoch_now(),
    ?Log("Waiting",[{min, (T-Now)/60},{time,calendar:local_time()}]),
    error_logger:info_report("Waiting",[{min, (T-Now)/60},{time,calendar:local_time()}]),

    if T>Now ->
            timer:sleep( max(T - Now,1) * 1000);
       true -> ok
    end.

update_needed(Actual,Stored) ->
    %% compare dates, format example: 2013-10-14T11:55:36Z
    Actual_date = calendar:datetime_to_gregorian_seconds(create_date(Actual)),
    Stored_date = calendar:datetime_to_gregorian_seconds(create_date(Stored)),
    if (Actual_date > Stored_date)       -> true;
       true -> false
    end.


create_date(Actual) ->
    [A_year,A_month,A_day,A_hour,A_minute,A_sec|_] = 
        string:tokens(Actual,"T-:Z"),
    {{to_integer(A_year),to_integer(A_month),to_integer(A_day)},
     {to_integer(A_hour),to_integer(A_minute),to_integer(A_sec)}}.

to_integer(S) ->
    {N,_}=string:to_integer(S),
    N.
