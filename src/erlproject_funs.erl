%%% @author Khashayar
%%% @copyright (C) 2013, Khashayar
%%% @doc
%%%     Pool of functions needed for parsing 
%%% @end
%%% Created : 24 Jul 2013 by Khashayar

-module(erlproject_funs).

-compile(export_all).

-include("records.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%%     Attempts to fetch and read a document from URL
%%% @end

read_web(git,{ok, {{_Version, _, _ReasonPhrase}, Headers, Body}}) ->
    case check(Headers) of
	ok -> try parse(mochijson:decode(Body)) of
                    no_result ->
                              {success, last, []};
                     Res ->
                              case proplists:get_value("link",Headers) of
                                  undefined ->
                                      {success, last, Res};
                                  Links ->
                                      {success, grab_next(git, Links), Res}
                              end
              catch
                  exit:_Exit ->
                                  {error,_Exit}
                end;
     error ->
                      {error,broken_html};
     Limit ->
                      {limit, Limit}


    end;

read_web(default,{ok, {{_Version, _, _ReasonPhrase}, Headers, Body}}) ->
    {success,{Headers,Body}};    
read_web(_,{error,socket_closed_remotely})->
    ?Log("read_web(error,socket_closed_remotely)", {reason,socket_closed_remotely}),
    {error,socket_closed_remotely};
read_web(_,{error,no_scheme})->
    ?Log("read_web(error,no_scheme)",{reason,no_scheme}),
    {error,broken_html};
read_web(_,{error,{failed_connect,_}})->
    ?Log("read_web(error,failed_connect)",{reason,connection_failed}),
    {error,connection_failed}; % broken link
read_web(_,{error,{ehostdown,_}})->
    ?Log("read_web(error,ehostdown)",{reason,host_is_down}),
    {error,host_is_down};
read_web(_,{error,{ehostunreach,_}})->
    ?Log("read_web(error,ehostunreach)",{reason,host_unreachable}),
    {error,host_unreachable};
read_web(_,{error,{etimedout,_}})->
    ?Log("read_web(error,etimedout)",{reason,connection_timed_out}),
    {error,connection_timed_out};
read_web(_,{error,timeout})->
    ?Log("read_web(error,timeout)",{reason,connection_timed_out}),
    {error,connection_timed_out};
read_web(_,{error,{ebadrqc,_}})->
    ?Log("read_web(error,ebadrqc)",{reason,bad_request_code}),
    {error,bad_request_code};
read_web(_,{error,{ecomm,_}})->
    ?Log("read_web(error,ecomm)",{reason,communication_error}),
    {error, communication_error};
read_web(_,{error,{econnrefused,_}})->
    ?Log("read_web(error,econnrefused)",{reason,connection_refused}),
    {error, connection_refused};
read_web(_,{error,{enetdown,_}})->
    ?Log("read_web(error,enetdown)",{reason,network_down}),
    {error, network_down};
read_web(_,{error,{enetunreach,_}})->
    ?Log("read_web(error,enetunreach)",{reason,network_unreachable}),
    {error, network_unreachable};
read_web(git,Src) ->
    ssl:start(),
    inets:start(),
    {A,Code} = try
                   httpc:request(get, 
                                 {Src, [{"User-Agent","Jable"},
                                        {"Accept","application/vnd.github.preview"}
                                       ]}, 
                                 [{timeout,timer:seconds(20)}], []) of
                   Answer -> {Answer,ok}
               catch
                   exit:R -> {{error,R},exit};
                   error:R -> {{error,R},error}
               end,
    if not(Code == ok) ->
            ?Log("read_web(git,src)",[{reason,Src},{answer,Code},{module, ?MODULE},{line,?LINE}]);
       true -> ok
    end,
    read_web(git,A);

read_web(default,{error,Reason}) ->
     ?Log("read_web(default,{error,Reason})",[{error,Reason},{module, ?MODULE},{line,?LINE}]),
     ?Log("Waiting",[{sec, 60},{time,calendar:local_time()}]),
     {error, Reason};

read_web(default,Src) ->
    ssl:start(),
    inets:start(),
    {A,Code} = try
                   httpc:request(get, 
                                 {Src, [{"User-Agent","Jable"}
                                       ]}, 
                                 [{timeout,timer:seconds(30)}], []) of
                   Answer -> {Answer,ok}
               catch
                   exit:R -> {{error,R},exit};
                   error:R -> {{error,R},
                               error}
               end,
    if not(Code == ok) ->
            ?Log("read_web(default,src)",[{reason,Src},{answer,Code},{module, ?MODULE},{line,?LINE}]);
       true -> ok
    end,
   % Need to handle the following case:
   % A =          {error,
             %% {could_not_parse_as_http,
             %%     <<"HTTP/1.1 0\r\nDate: Wed, 08 Jan 2014 21:48:20 GMT\r\nStatus: 0\r\nConnection: close\r\nContent-Type: text/html;charset=utf-8\r\nX-RateLimit-Limit: 5000\r\nX-RateLimit-Remaining: 4871\r\nX-RateLimit-Reset: 1389221128\r\nX-Content-Type-Options: nosniff\r\nContent-Length: 0\r\nAccess-Control-Allow-Credentials: true\r\nAccess-Control-Expose-Headers: ETag, Link, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset, X-OAuth-Scopes, X-Accepted-OAuth-Scopes, X-Poll-Interval\r\nAccess-Control-Allow-Origin: *\r\nX-GitHub-Request-Id: 51B7B182:06F3:7FF9E89:52CDC79A\r\n\r\n">>}}},
    read_web(default,A);

read_web(_,Reason) ->
    ?Log("read_web(_,Reason)",{reason,Reason}),
             {error,Reason}.

check(Header) ->
    case proplists:get_value("status", Header) of
	"200 OK" ->
	    ok;
	"403 Forbidden" ->
	    T = proplists:get_value("x-ratelimit-reset",Header),
	    Limit = list_to_integer(T),
            Now = erlproject_cunit:epoch_now(),
            if Limit < Now  -> error;
               true ->
                   %%  error_logger:info_report(["check(Header) resulted T",{"x-ratelimit-reset",T},{header, Header}]),
                    Limit
            end;
	_ ->
	    error
    end.


%%%-------------------------------------------------------------------
%%% @doc
%%%     Parsing material for json and html 
%%% @end
%% {struct,[{"total_count",763},
%%          {"incomplete_results",false},
%%          {"items",
%%           {array,[{struct,[{"id",374927},
%%                            {"name","otp"},
%%                            {"full_name","erlang/otp"},
%%                            {"owner",
%%                             {struct,[{"login","erlang"},
%%                                      {"id",153393},
%% ...

parse({struct,Struct}) ->
    case lists:keysearch("total_count",1,Struct) of
        false ->
            {error,json_failed};
        {value,{_,0}} ->    
            no_result;
        {value,{_,_N}} ->
            case lists:keysearch("items",1,Struct) of
                false ->
                    {error,json_failed};
                {value,{_,{array,List}}} ->
                    List;
                _Result ->
                    {error,json_failed}
            end
    end;

%% % parse({struct,[{_,0},{_,{array, _List}}]}) ->
%% parse({struct,[{_,0},_,{_,{array, _List}}]}) ->
%%     no_result;
%% % parse({struct,[{_,_X},{_,{array, List}}]}) ->
%% parse({struct,[{_,_X},_,{_,{array, List}}]}) ->
%%     List;
parse(_) ->
    {error,json_failed}.

extract(git, {struct, List}) ->
    #git{id = proplists:get_value("id",List),
	 name = proplists:get_value("name",List),
	 full_name = proplists:get_value("full_name",List),
	 owner = extract_owner(proplists:get_value("owner",List)),
	 html_url = proplists:get_value("html_url",List),
	 description = proplists:get_value("description",List),
	 languages_url = proplists:get_value("languages_url",List),
	 commits_url = proplists:get_value("commits_url",List),
	 languages = proplists:get_value("language",List),
	 created_at = proplists:get_value("created_at",List),
	 updated_at = proplists:get_value("updated_at",List),
	 pushed_at = proplists:get_value("pushed_at",List),
	 clone_url = proplists:get_value("clone_url",List),
	 watchers = proplists:get_value("watchers",List),
	 open_issues = proplists:get_value("open_issues",List),
	 forks = proplists:get_value("forks",List)};
extract(git_commit, {struct, List}) ->
    {struct, Commit} = proplists:get_value("commit", List),
    {struct, Author} = proplists:get_value("author", Commit),
    #commit{sha = proplists:get_value("sha", List),
	    author = proplists:get_value("name", Author),
	    date = proplists:get_value("date", Author),
	    url = proplists:get_value("html_url", List),
	    message = proplists:get_value("message", Commit)};
extract(google ,{_, _, [{<<"tr">>,_,[{<<"td">>,_,_},{<<"td">>,_,Data}]}]}) ->
    extract(google, Data);
extract(google , [Name_Link, _ , Star_Data , _ , DescT|_]) ->
    [{_,Attrs,Val}] = get_value([Name_Link], "a", []),
    case Star_Data of
	{_,_,[Updated,{_,_,[Stars]}]} ->
	    DescIn = DescT;
	{_,_,[Updated|_]} ->
	    Stars = <<"0">>,
	    DescIn = DescT;
	X ->
	    Stars = <<"0">>,
	    Updated = <<"undef">>,
	    DescIn = X
    end,
    Desc = case DescIn of
        D when is_bitstring(D) == true -> D;
        [] -> <<"undef">>;
        _D1 ->  %% error_logger:info_report(["Desc ",{desc,_D1}]),
                   <<"undef">>
    end,
    Html_Url = proplists:get_value(<<"href">>,Attrs),
    Full_Name = hd(Val),
    [_,Name] = string:tokens(bitstring_to_list(Html_Url),"/"),
    {Updated_at,_} = translate_update(bitstring_to_list(Updated)),
    #git{id = 1,
	 name = Name,
	 full_name = bitstring_to_list(Full_Name),
	 owner = undef,
	 html_url = "https://code.google.com" ++ 
	     bitstring_to_list(Html_Url),
	 description = bitstring_to_list(Desc),
	 languages = "Erlang",
	 created_at = undef,
	 updated_at = {Updated_at,{0,0,0}},
	 pushed_at = undef,
	 watchers = list_to_integer(bitstring_to_list(Stars)),
	 open_issues = undef,
	 forks = undef};
extract(sfapi, {struct,[{_,{struct,List}}]}) ->
    TS = proplists:get_value("created_timestamp",List),
    C = calendar:gregorian_seconds_to_datetime(TS+(719528*24*3600)),
    #git{id = proplists:get_value("id",List),
	 name = proplists:get_value("shortdesc",List),
	 full_name = proplists:get_value("name",List),
	 html_url = proplists:get_value("summary-page",List),
	 description = proplists:get_value("description",List),
	 languages = proplists:get_value("programming-languages",List),
	 created_at = C,
	 watchers = 0};
extract(bbapi, {struct, List}) ->
                                                %name,full_name, owner, html_url, description, languages,  created_at,
                                                %updated_at,  watchers , forks
    Name = proplists:get_value("slug",List),
    Owner = proplists:get_value("owner",List),
    #git{id = 2,
	 name = Name,
	 full_name = proplists:get_value("name",List),
	 html_url = "https://bitbucket.org/" ++ Owner ++ "/" ++ Name,
	 description = proplists:get_value("description",List),
	 languages = proplists:get_value("language",List),
	 created_at = proplists:get_value("utc_created_on",List),
	 updated_at = proplists:get_value("utc_last_updated",List),
	 watchers = proplists:get_value("followers_count",List),
	 forks = proplists:get_value("forks_count",List)};
extract(_,_) ->
    not_valid.



extract_owner({struct, List}) ->
    #owner{login = proplists:get_value("login",List),
	   id = proplists:get_value("id",List),
	   avatar_url = proplists:get_value("avatar_url",List),
	   url = proplists:get_value("html_url",List)}.


%%%-------------------------------------------------------------------
%%% @doc
%%%     A Link generator for different sources
%%% @end
source_gen({{Year,Month,_},_}) ->
     [bitbucket,google]++
         source_gen(2010,1,Year,Month,[{l,"<2010-01-01"},{s,"<2010-01-01"}]).
%% source_gen({{Year,Month,_},_}) ->
%%    source_gen(2010,1,Year,Month,[{l,"<2010-01-01"},{s,"<2010-01-01"}]).
source_gen(Y,M,Y,M,Buff)->
    Buff ++ [{l,">"++date_format(Y,M)},{s,">"++date_format(Y,M)}]; 
	     

%% temporary fix to test exit erlproject_unit after the last item is parsed
%% source_gen({{Year,Month,_},_}) ->
%%     [sourceforge,bitbucket].


source_gen(Y,12,TY,TM,Buff) ->
    source_gen(Y+1,1,TY,TM,Buff ++ 
                   [{l,date_format(Y,12)++".."++date_format(Y+1,1)},
                    {s,date_format(Y,12)++".."++date_format(Y+1,1)}]);
source_gen(Y,M,TY,TM,Buff) ->
    source_gen(Y,M+1,TY,TM,Buff ++ 
		   [{l,date_format(Y,M)++".."++date_format(Y,M+1)},
		    {s,date_format(Y,M)++".."++date_format(Y,M+1)}]).

date_format(Y,M) when M < 10->
    integer_to_list(Y)++"-"++integer_to_list(0)++integer_to_list(M)++"-01";
date_format(Y,M) ->
    integer_to_list(Y)++"-"++integer_to_list(M)++"-01".



%%%-------------------------------------------------------------------
%%% @doc
%%%     Grabbing next Urls of the search
%%% @end


grab_next(git,Links) ->
    case string:str(Links,"rel=\"next\"") of
	0 ->
	    last;
	N ->
	    S = string:substr(Links,1,N),   
	    Start = string:rstr(S,"<"),
	    End = string:rstr(S,">"), 
	    string:substr(S,Start+1,End-Start-1)
    end;

grab_next(google,[]) ->
    last;
grab_next(google,[{_,Attr,[<<"Next ">>|_]}|_T]) ->
    "https://code.google.com/hosting/" ++
	bitstring_to_list(proplists:get_value(<<"href">>,Attr));
grab_next(google,[_|T]) ->
    grab_next(google,T);

grab_next(sf,[]) ->
    last;
grab_next(sf, [{_,Attr,[<<"Next">>|_]}|_T]) ->
    "http://sourceforge.net" ++
	bitstring_to_list(proplists:get_value(<<"href">>,Attr));
grab_next(sf, [_|T]) ->
    grab_next(sf, T);

grab_next(bitbucket,[]) ->
    last;
grab_next(bitbucket, [{_,Attr,[<<"Next">>|_]}|_T]) ->
    case bitstring_to_list(proplists:get_value(<<"href">>,Attr)) of
	"#" -> 
	    last;
	Next ->
	    "https://bitbucket.org" ++ Next
    end;
grab_next(bitbucket, [_|T]) ->
    grab_next(bitbucket, T).



%%%-------------------------------------------------------------------
%%% @doc
%%%     For parsing mochiweb parsed html format
%%% @end

get_value([{Key,Attr,Val=[{_IK,_IA,_IV}|_IT]}|T] , Filter , Buff) ->
    case bitstring_to_list(Key) == Filter of
	true ->
	    get_value(T,Filter,[{Key,Attr,Val}|Buff]);
	false ->
	    get_value(T,Filter,get_value(Val,Filter,[])++Buff)
    end;
get_value([{Key,Attr,Val = [_|IT]}|T] , Filter , Buff) ->
    case bitstring_to_list(Key) == Filter of
	true ->
	    get_value(T,Filter,[{Key,Attr,Val}|Buff]);
	false ->
	    get_value(T,Filter,get_value(IT,Filter,[])++Buff)
    end;
get_value([{Key,Attr,Val}|T] , Filter , Buff) ->
    case bitstring_to_list(Key) == Filter of
	true ->
	    get_value(T,Filter,[{Key,Attr,Val}|Buff]);
	false ->
	    get_value(T,Filter,Buff)
    end;
get_value([_|T] , Filter , Buff) ->
    get_value(T,Filter,Buff);
get_value([] , _ , Buff) ->
    Buff.

get_content(List,Filter,Value) ->
    get_content(List,Filter,Value,[]).

get_content([{_,Attr,_} | T] , Filter , Value , Buff) ->
    case check_content(Attr,Filter) of 
	true ->
	    Res = pull_content(Attr,Value),
	    get_content(T,Filter,Value , [Res | Buff]);
	false ->
	    get_content(T,Filter,Value , Buff)
    end;

get_content([],_,_ , Buff) ->
    Buff. 


pull_content([{Key,Val}|T] , FKey) ->
    case bitstring_to_list(Key) == FKey of
	true ->
            bitstring_to_list(Val);
	false ->
	    pull_content(T,FKey)
    end;
pull_content([] , _) ->
    not_found.

check_content([{Key,Val}|T] , {FKey , FVal}) ->
    KeyCheck = bitstring_to_list(Key) == FKey,
    ValCheck = bitstring_to_list(Val) == FVal,
    case {KeyCheck , ValCheck} of
	{true,true} ->
	    true;
	{_,_} ->
	    check_content(T , {FKey , FVal})
    end;
check_content([],_) ->
    false.


%%%-------------------------------------------------------------------
%%% @doc
%%%     For Translating date format of the google
%%% @end

translate_update(Date)->
    case io_lib:fread("\n Updated: ~s",Date) of
	{ok, ["Earlier"], _} ->
	    Time = erlang:localtime(), 
	    Now = calendar:datetime_to_gregorian_seconds(Time),
	    calendar:gregorian_seconds_to_datetime(Now - 31536000);
	{ok, ["Today"],_} ->
	    erlang:localtime(); 
	{ok, ["Last"],_} ->
	    Time = erlang:localtime(), 
	    Now = calendar:datetime_to_gregorian_seconds(Time),
	    calendar:gregorian_seconds_to_datetime(Now - 2592000);
	{ok, _, _} ->
	    case io_lib:fread("\n Updated: ~3s ~2d, ~4d",Date) of 
		{ok,[Month,Day,Year],_} ->
		    {{Year,month_num(Month),Day},{0,0,0}};
		_Reason ->
		    {{1970,1,1},{0,0,0}}
	    end;
	{more, _RestFormat, _Nchars, _InputStack} ->
	    {{1970,1,1},{0,0,0}};
	{error, _Reason}  ->
	    {{1970,1,1},{0,0,0}}
    end.



month_num("Jan") ->
    1;
month_num("Feb") ->
    2;
month_num("Mar") ->
    3;
month_num("Apr") ->
    4;
month_num("May") ->
    5;
month_num("Jun") ->
    6;
month_num("Jul") ->
    7;
month_num("Aug") ->
    8;
month_num("Sep") ->
    9;
month_num("Oct") ->
    10;
month_num("Nov") ->
    11;
month_num("Dec") ->
    12;
month_num(_) ->
    0.

connect() ->
   case whereis(erlproject_cunit) of
       undefined -> 
                                                % initial start don't need to do anything
           ok;
       _Pid -> 
                                                % erlproject_db was crashed -> 
                                                %  if parser crashed as well it needs to be restarted
           ?Log("send continue_parsing message",[]),
           timer:sleep(1000), 
                                                %% wait a bit while mysql can be up again
           gen_server:cast(erlproject_cunit, {continue_parsing})
   end,

   ?Log("erlproject_db:connect",[]),
     %% start creates a gen_server process which is not
     %% part of a supervision tree
    try 
        case emysql:add_pool(p1, 1,  ?USER, ?PWD, ?HOST, ?PORT,?PROJECT, utf8) of
            ok ->  {ok, whereis(emysql_conn_mgr)};
            {reply, ok, _ } ->
                {ok, whereis(emysql_conn_mgr)};
            {reply, {error, pool_already_exists}, _} ->
                {error, already_started};
            {error,pool_already_exist} ->
                {ok, whereis(emysql_conn_mgr)};
            {_,Reason} -> {error,Reason}
        end
    catch
        exit:pool_already_exists ->
            {ok,  whereis(emysql_conn_mgr)}
    end.

            
                
%% Return a list of all tcp sockets
tcp_sockets() -> port_list("tcp_inet").
udp_sockets() -> port_list("udp_inet").
sctp_sockets() -> port_list("sctp_inet").

%% Return all ports having the name 'Name'
port_list(Name) ->
    lists:filter(
      fun(Port) ->
	      case erlang:port_info(Port, name) of
		  {name, Name} -> true;
		  _ -> false
	      end
      end, erlang:ports()).
