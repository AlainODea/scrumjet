%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Boards Resource.

-module(scrumjet_boards_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1]).
%% representation generators
-export([to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, Context) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Boards - ScrumJet</title>
</head>
<body>
<h1>ScrumJet Boards</h1>
<ul id='boards'>
">>,
boards(),
<<"
</ul>
</body>
</html>
">>], ReqData, Context}.

-spec boards() -> iolist().
boards() ->
    {atomic, List} = mnesia:transaction(fun() ->
        mnesia:foldl(fun scrumjet_html:li/2, [], scrumjet_board)
    end),
    List.
