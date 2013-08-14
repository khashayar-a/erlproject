%%%-------------------------------------------------------------------
%%% @author Khashayar <khashayar@khashayar>
%%% @copyright (C) 2013, Khashayar
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2013 by Khashayar <khashayar@khashayar>
%%%-------------------------------------------------------------------
-module(erlproject_parser).

-export([start/1, init/1, crawl/2, parse/2]).

-include("records.hrl").

start(Url) ->
    spawn(erlproject_parser, init, [Url]).

init({Source,Url}) ->
    crawl(Source,Url).

crawl(git, Url) ->
    case erlproject_funs:read_web(git,Url) of
	{limit, T} ->
	    gen_server:cast(erlproject_cunit, {wait, T, {git,Url}}); 
	{success, last, List} ->
    	    gen_server:cast(erlproject_cunit, last),
	    parse(git, List);
	{success, Next, List} ->
	    gen_server:cast(erlproject_cunit, {next, {git,Next}}),
	    parse(git, List);
	{error,Reason} ->
	    gen_server:cast(erlproject_cunit, {error, {Reason,Url}});
	_ ->
	    io:format("SOMETHING IS HAPPENING ~p~n" , [Url])
    end;
crawl(git_language, Url) ->
    case erlproject_funs:read_web(default,Url) of
	{success,{Headers,Body}} ->
	    case erlproject_funs:check(Headers) of 
		ok ->
		    L = string:tokens(Url, "/"),
		    Name = lists:nth(4,L) ++ "/" ++ lists:nth(5,L),
		    gen_server:cast(erlproject_db,
				    {write, git_language, {Body,Name}});
		error ->
		    gen_server:cast(erlproject_cunit, 
				    {error, {headers,Url}});		
		Limit ->
		    gen_server:cast(erlproject_cunit, 
				    {wait, Limit, {git_language,Url}})
	    end;
	{error, Reason} ->
	    gen_server:cast(erlproject_cunit, {error, {Reason,Url}})
    end;    
crawl(git_commit, Url) ->
    case erlproject_funs:read_web(default,Url) of
	{success,{Headers,Body}} ->
	    case erlproject_funs:check(Headers) of 
		ok ->
		    {array,List} = mochijson:decode(Body),
		    parse(git_commit, List);
		error ->
		    gen_server:cast(erlproject_cunit, 
				    {error, {headers,Url}});		
		Limit ->
		    gen_server:cast(erlproject_cunit, 
				    {wait, Limit, {git_commit,Url}})
	    end;
	{error, Reason} ->
	    gen_server:cast(erlproject_cunit, {error, {Reason,Url}})
    end;    
crawl(google, Url) ->
    case erlproject_funs:read_web(default,Url) of
	{success,{_Headers,Body}} ->
	    Html = mochiweb_html:parse(Body),
	    T = erlproject_funs:get_value([Html], "a", []),
	    case erlproject_funs:grab_next(google, T) of
		last ->
		    gen_server:cast(erlproject_cunit, last);
		Next ->
		    gen_server:cast(erlproject_cunit, 
				    {next, {google,Next}})
	    end,
	    parse(google,Html);
	{error, Reason} ->
	    gen_server:cast(erlproject_cunit, {error, {Reason,Url}})
    end;
crawl(sourceforge, Url) ->
     case erlproject_funs:read_web(default,Url) of
	{success,{_Headers,Body}} ->
	    Html = mochiweb_html:parse(Body),
	    Links = erlproject_funs:get_value([Html], "a", []),
	    case erlproject_funs:grab_next(sf, Links) of
		last ->
		    gen_server:cast(erlproject_cunit, last);
		Next ->
		    gen_server:cast(erlproject_cunit, 
				    {next, {sourceforge,Next}})
	    end,
	    parse(sourceforge,Links);
	{error, Reason} ->
	    gen_server:cast(erlproject_cunit, {error, {Reason,Url}})
     end;
crawl(sfapi,Url) ->
     case erlproject_funs:read_web(default,Url) of
	{success,{_Headers,Body}} ->
	     parse(sfapi,Body);
	 {error, Reason} ->
	     gen_server:cast(erlproject_cunit, {error, {Reason,Url}})
     end;
crawl(bitbucket, Url) ->
     case erlproject_funs:read_web(default,Url) of
	{success,{_Headers,Body}} ->
	    Html = mochiweb_html:parse(Body),
	    Links = erlproject_funs:get_value([Html], "a", []),
	    case erlproject_funs:grab_next(bitbucket, Links) of
		last ->
%		    io:format("Last ~p~n",[[]]);
		    gen_server:cast(erlproject_cunit, last);
		Next ->
%		    io:format("Next ~p~n",[Next])
		    gen_server:cast(erlproject_cunit, 
				    {next, {bitbucket,Next}})
	    end,
	    parse(bitbucket, Links);
	{error, Reason} ->
	    gen_server:cast(erlproject_cunit, {error, {Reason,Url}})
     end;
crawl(bbapi,Url) ->
     case erlproject_funs:read_web(default,Url) of
	 {success,{_Headers,Body}} ->
	     parse(bbapi,Body);
	 {error, Reason} ->
	     gen_server:cast(erlproject_cunit, {error, {Reason,Url}})
     end.
	 
parse(git, List) ->
    Auth = "?access_token=e62fdebb6e20c178dd30febcc7126e06367dd975",
    Extract = fun(X) -> 
		      erlproject_funs:extract(git,X) 
	      end, 
    Res = lists:map(Extract , List),
    Cast = fun(X) -> 
		   Languages = X#git.languages_url ++ Auth,
		   Commits = hd(string:tokens(X#git.commits_url, "{")) 
					   ++ Auth ++ "&per_page=3",
		   gen_server:cast(erlproject_cunit, 
				   {language, {git_language, Languages}}),
%		   gen_server:cast(erlproject_cunit, 
%				   {commit, {git_commit, Commits}}),
		   gen_server:cast(erlproject_db,{write, git,  X}) 
	   end,
    lists:foreach(Cast, Res);

parse(git_commit, List) ->
    Extract = fun(X) ->
		      erlproject_funs:extract(git_commit, X)
	      end,
    Res = lists:map(Extract, List),
    commiter(Res);

%    A = hd(Res),
%    Url =  A#commit.url,
%    L = string:tokens(Url, "/"),
%    Name = lists:nth(3,L) ++ "/" ++ lists:nth(4,L),
%    Delete = gen_server:call(erlproject_db, {delete, commit, Name}),
%    case Delete of
%	ok ->
%	    Cast = fun(X) ->
%			   gen_server:cast(erlproject_db, 
%					   {write, git_commit, X})
%		   end,
%	    lists:foreach(Cast, Res);
%	error ->
%	    error
%    end;
	
parse(google,Html) ->
    T = erlproject_funs:get_value([Html], "table", []),
    Extract = fun(X) -> 
		      erlproject_funs:extract(google,X) 
	      end, 
    Res = lists:map(Extract , T),
    Cast = fun(X) ->
		   case X of
		       not_valid ->
			   ok;
		       Elem ->
			   gen_server:cast(erlproject_db,
					   {write, google,  Elem}) 
		   end
	   end,
    %test(Res,0),
    lists:foreach(Cast, Res);
parse(sourceforge, Links) ->
    Projects = erlproject_funs:get_content(Links,
					   {"class" ,"project-icon"},
					   "href"),
    Extract = fun(X) ->
		      Name =lists:nth(2, string:tokens(X,"/")),
		      "http://sourceforge.net/api/project/name/" ++
			  Name ++ "/json"
	      end,
    Res = lists:map(Extract,Projects),
    Spawn = fun(X) ->
		    erlproject_parser:start({sfapi,X})
	    end,
    lists:foreach(Spawn, Res);
parse(sfapi,Body) ->
    case erlproject_funs:extract(sfapi, mochijson:decode(Body)) of
	Data = #git{} ->
	    gen_server:cast(erlproject_db, {write, sfapi,  Data}); 
	Reason ->
	    error_logger:info_report(["Pasing SF MOCHI",
				      {reason,Reason}])	   

    end;
parse(bitbucket, Links) ->
    Projects = erlproject_funs:get_content(Links,
					   {"class" ,"avatar-link"},
					   "href"),
    Extract = fun(X) ->
		      "https://bitbucket.org/api/1.0/repositories" ++
			  X
	      end,
    Res = lists:map(Extract,Projects),
    Spawn = fun(X) ->
		    erlproject_parser:start({bbapi,X})
	    end,
    lists:foreach(Spawn, Res);
parse(bbapi,Body) ->
    case erlproject_funs:extract(bbapi, mochijson:decode(Body)) of
	Data = #git{} ->
	    gen_server:cast(erlproject_db, {write, bbapi,  Data}); 
	Reason ->
	    error_logger:info_report(["Pasing BB MOCHI",
				      {reason,Reason}])	   

    end.



commiter(List) ->
    commiter(List, 1).

commiter([], _) ->
    ok;
commiter([H|T],N) ->
    gen_server:cast(erlproject_db, 
		    {write, git_commit, {N,H}}),
    commiter(T,N+1).
	

test([],S) ->
    io:format(" ~p Messages Sent ~n" , [S]);
test([not_valid|T],S) ->
    test(T,S);
test([H|T],S) ->
    %io:format(" ~p~n" , [H]),
    test(T,S+1).


