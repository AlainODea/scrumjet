%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_tasks_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').

%% webmachine resource functions
-export([
    init/1,
    content_types_provided/2
    ]).

%% content generators
-export([
    to_html/2,
    to_json/2
    ]).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id}).

init([]) -> {ok, #context{}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}, {"application/json", to_json}],
        ReqData, Context}.

to_html(ReqData, Context) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Tasks - ScrumJet</title>">>,
html:head(),
<<"
</head>
<body style='display:none'>
<h1>ScrumJet Tasks</h1>
<ul id='tasks'>">>,
lists:foldl(fun html:li/2, [], tasks()),
<<"
</ul>
</body>
</html>
">>], ReqData, Context}.

to_json(ReqData, Context) ->
    {mochijson2:encode([json:value(T) || T <- tasks()]),
        ReqData, Context}.

-spec tasks() -> [sj_record()].
tasks() ->
    {atomic, List} = mnesia:transaction(fun() ->
        mnesia:foldl(fun(X,Xs) -> [X|Xs] end, [], scrumjet_task)
    end),
    List.
