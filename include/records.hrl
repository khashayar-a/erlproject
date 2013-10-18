-record(git, {id, name, full_name, owner, html_url, 
	      description, languages_url, commits_url,
	      languages, created_at, updated_at,
	      pushed_at, clone_url, watchers, open_issues, forks}).

-record(owner, {login, id, avatar_url, url}).

-record(commit, {sha, author, date, url, message}).

- ifdef(debug).
- define(L(Msg,Prameters), error_logger:info_report([Msg,Parameters])).
- else.
- define(L(Msg,Prameters), ok).
- endif.


-define(SOURCEFORGE_OVERLOAD, "Too many requests, please try again later.").
