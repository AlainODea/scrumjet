%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_tasks_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').

%% webmachine resource functions
-export([init/1]).

%% content generators
-export([to_html/2]).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id}).

init([]) -> {ok, #context{}}.

to_html(ReqData, Context) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Tasks - ScrumJet</title>
</head>
<body>
<h1>ScrumJet Tasks</h1>
<ul id='tasks'>
">>,
tasks(),
<<"
</ul>
</body>
</html>
">>], ReqData, Context}.

-spec tasks() -> iolist().
tasks() ->
    {atomic, List} = mnesia:transaction(fun() ->
        mnesia:foldl(fun html:li/2, [], scrumjet_task)
    end),
    List.
