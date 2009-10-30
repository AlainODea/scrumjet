%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Category-Task Relationship Resource.

-module(scrumjet_category_task_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1]).
%% resource functions
-export([resource_exists/2,
         allowed_methods/2,
         content_types_accepted/2]).
%% resource generators
-export([to_html/2]).
%% resource parsers
-export([from_any/2]).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(context, {record :: #scrumjet_category_task{}}).

init([]) -> {ok, #context{}}.

resource_exists(ReqData, Context) ->
    DispatchPath = wrq:disp_path(ReqData),
    case string:tokens(DispatchPath, ";") of
        [ID, TaskID] ->
            case scrumjet_category_task:find({id, ID, TaskID}) of
                [] ->
                    Template = #scrumjet_category_task{id=ID, task_id=TaskID},
                    {false, ReqData, Context#context{record=Template}};
                [CategoryTask] ->
                    {true, ReqData, Context#context{record=CategoryTask}}
            end;
        _ -> {{halt, 400}, ReqData, Context}
    end.

to_html(ReqData, Context=#context{record=
        #scrumjet_category_task{id=ID, task_id=TaskID}}) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Category/Task ID: ">>,ID,<<"/">>,TaskID,<<" - ScrumJet</title>
<body>
<a id='task' href='">>,scrumjet_uri:for(#scrumjet_task{id=TaskID}),
<<"'>Task ">>,TaskID,<<"</a> is in <a id='category'
href='">>,scrumjet_uri:for(#scrumjet_category{id=ID}),<<"'>Category ">>,ID,<<"</a>
</body>
</html>
">>], ReqData, Context}.

allowed_methods(ReqData, Context) -> {['GET', 'HEAD', 'PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) -> {[{
    case wrq:get_req_header("content-type", ReqData) of
        undefined -> "application/octet-stream";
        X -> X
    end, from_any}], ReqData, Context}.

%% to relate a category and a task you simply assert that the relationship
%% exists by PUTting it there
from_any(ReqData, Context=#context{record=CategoryTask}) ->
    scrumjet_category_task:store(CategoryTask),
    {true, ReqData, Context}.
