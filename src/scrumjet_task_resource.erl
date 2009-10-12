%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_task_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1]).

%% webmachine resource functions
-export([
    allowed_methods/2,
    resource_exists/2,
    content_types_accepted/2,
    content_types_provided/2
    ]).

%% content generators
-export([
    to_html/2,
    to_json/2,
    from_webform/2
    ]).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {task :: #scrumjet_task{}}).

init([]) -> {ok, #context{}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}, {"application/json", to_json}],
        ReqData, Context}.

resource_exists(ReqData, Context) ->
    ID = wrq:disp_path(ReqData),
    case scrumjet_datastore:find(#scrumjet_task{id=ID}) of
        [] -> {false, ReqData, Context#context{task=#scrumjet_task{id=ID}}};
        [Task] -> {true, ReqData, Context#context{task=Task}}
    end.

to_html(ReqData, Context=#context{task=#scrumjet_task{id=ID,
        headline=Headline, description=Description}}) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Task ID: ">>,ID,<<" - ScrumJet</title>">>,
html:head(),
<<"<script type='text/javascript' src='/static/task.js'></script>
</head>
<body style='display:none'>
<h1 id='headline'>">>,Headline,<<" (">>,ID,<<")</h1>
<p id='description'>">>,Description,<<"</p>
<h2>Categories</h2>
<ul id='categories'>">>,
categories(ID),
<<"
</ul>
</body>
</html>
">>], ReqData, Context}.

to_json(ReqData, Context=#context{task=Task}) ->
    {mochijson2:encode(json:value(Task)), ReqData, Context}.

%% should accept PUT requests to create new tasks
allowed_methods(ReqData, Context) -> {['GET', 'HEAD', 'PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/x-www-urlencoded", from_webform}], ReqData, Context}.

from_webform(ReqData, Context=#context{task=Task}) ->
    Params = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    Headline = proplists:get_value("headline", Params, ""),
    Description = proplists:get_value("description", Params, ""),
    scrumjet_datastore:store(
        Task#scrumjet_task{description=Description,
                           headline=Headline}),
    {true, ReqData, Context}.

-spec categories(string()) -> iolist().
categories(ID) ->
    lists:foldl(fun html:li/2, [],
        scrumjet_category_task:find({categories, ID})).
