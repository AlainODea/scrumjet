%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Boards Resource.

-module(scrumjet_boards_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(_ReqProps, State) ->
    {[<<"
<html>
<head>
<title>Boards - ScrumJet</title>
</head>
<body>
<h1>ScrumJet Boards</h1>
<ul>
">>,
ets:foldl(fun html:li/2, [], scrumjet_board),
<<"
</ul>
</body>
</html>
    ">>], State}.