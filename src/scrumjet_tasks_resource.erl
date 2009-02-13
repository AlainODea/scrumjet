%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_tasks_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(_ReqProps, State) ->
    {[<<"
<html>
<head>
<title>Tasks - ScrumJet</title>
</head>
<body>
<h1>ScrumJet Tasks</h1>
<ul>
">>,
ets:foldl(fun html:list_item/2, [], scrumjet_task),
<<"
</ul>
</body>
</html>
">>], State}.

%% should NOT accept POST to create new tasks (not idempotent, problematic for reliability)