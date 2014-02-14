-record(git, {id, name, full_name, owner, html_url, 
	      description, languages_url, commits_url,
	      languages, created_at, updated_at,
	      pushed_at, clone_url, watchers, open_issues, forks}).

-record(owner, {login, id, avatar_url, url}).

-record(commit, {sha, author, date, url, message}).

- ifdef(debug1).
- define(L(Msg,Parameters), error_logger:info_report([Msg,Parameters])).
- else.
- define(L(Msg,Parameters), ok).
- endif.

- ifdef(debug1).
- define( Log(Msg,Parameters), error_logger:info_report([Msg,Parameters])).
- else.
- define( Log(Msg,Parameters), ok).
- endif.

-define(SOURCEFORGE_OVERLOAD, "Too many requests, please try again later.").
%% Khashayar's token
-define(GITHUB_ACCESS_TOKEN, "e62fdebb6e20c178dd30febcc7126e06367dd975").
%% Eva's access token
%% -define(GITHUB_ACCESS_TOKEN, "743619cbf02d739b3ed678ec6d748afb394618f4").

%% mySQL settings for local computer:

-define(HOST, "127.0.0.1").
-define(PORT, 3306).
-define(USER, "evabihari").
-define(PWD,"ethebi1").
-define(PROJECT,"erlproject").
