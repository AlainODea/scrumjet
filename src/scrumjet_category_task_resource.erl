%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Category Resource.

-module(scrumjet_category_task_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1]).
%% webmachine resource API
-export([
        resource_exists/2,
        % service_available/2,
        % is_authorized/2,
        % forbidden/2,
        % allow_missing_post/2,
        % malformed_request/2,
        % uri_too_long/2,
        % known_content_type/2,
        % valid_content_headers/2,
        % valid_entity_length/2,
        % options/2,
        allowed_methods/2,
        % delete_resource/2,
        % delete_completed/2,
        % post_is_create/2,
        % create_path/2,
        % process_post/2,
        % content_types_provided/2,
        content_types_accepted/2
        % charsets_provided/2,
        % encodings_provided/2,
        % variances/2,
        % is_conflict/2,
        % multiple_choices/2,
        % previously_existed/2,
        % moved_permanently/2,
        % moved_temporarily/2,
        % last_modified/2,
        % expires/2,
        % generate_etag/2,
        % finish_request/2
        ]).
%% handlers
-export([to_html/2, from_any/2]).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(context, {category_task}).

init([]) -> {ok, #context{}}.

resource_exists(ReqData, Context) ->
    DispatchPath = wrq:disp_path(ReqData),
    case string:tokens(DispatchPath, ";") of
        [ID, TaskID] ->
            case scrumjet_category_task:find({id, ID, TaskID}) of
                [] -> {false, ReqData, Context#context{category_task=#scrumjet_category_task{id=ID, task_id=TaskID}}};
                [CategoryTask] -> {true, ReqData, Context#context{category_task=CategoryTask}}
            end;
        _ -> {{halt, 400}, ReqData, Context}
    end.

%% TODO: represent as a pair of links: one to the category, one to the task
to_html(ReqData, Context=#context{category_task=#scrumjet_category_task{id=ID, task_id=TaskID}}) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Category/Task ID: ">>,ID,<<"/">>,TaskID,<<" - ScrumJet</title>
<body>
<a href=\"">>,uri:for(#scrumjet_task{id=TaskID}),<<"\">Task ">>,TaskID,<<"</a>
is in <a href=\"">>,uri:for(#scrumjet_category{id=ID}),<<"\">Category ">>,ID,<<"</a>
</body>
</html>
">>], ReqData, Context}.

%% should accept PUT requests to create new tasks
allowed_methods(ReqData, Context) -> {['GET', 'HEAD', 'PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) -> {[{
    case wrq:get_req_header("content-type", ReqData) of
        undefined -> "application/octet-stream";
        X -> X
    end, from_any}], ReqData, Context}.

%% to relate a category and a task you simply assert that the relationship
%% exists by PUTting it there
from_any(ReqData, Context=#context{category_task=CategoryTask}) ->
    scrumjet_category_task:store(CategoryTask),
    {true, ReqData, Context}.
