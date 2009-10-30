%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Categories Resource.

-module(scrumjet_categories_resource).
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
<title>Categories - ScrumJet</title>
</head>
<body>
<h1>ScrumJet Categories</h1>
<ul id='categories'>
">>,
categories(),
<<"
</ul>
</body>
</html>
">>], ReqData, Context}.

-spec categories() -> iolist().
categories() ->
    {atomic, List} = mnesia:transaction(fun() ->
        mnesia:foldl(fun scrumjet_html:li/2, [], scrumjet_category)
    end),
    List.