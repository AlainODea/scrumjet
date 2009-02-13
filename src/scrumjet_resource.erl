%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Home.

-module(scrumjet_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(_ReqProps, State) ->
    {<<"
<html>
<head>
<title>Welcome! - ScrumJet</title>
</head>
<body>
<h1>Welcome to ScrumJet!</h1>
<ul>
<li><a href=\"boards/\">Boards</a></li>
<li><a href=\"categories/\">Categories</a></li>
<li><a href=\"tasks/\">Tasks</a></li>
</ul>
</body>
</html>
">>, State}.
