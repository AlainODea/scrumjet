%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_tasks_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').

%% webmachine resource functions
-export([
        % ping/2,
        % service_available/2,
        % resource_exists/2,
        % auth_required/2,
        % is_authorized/2,
        % forbidden/2,
        % allow_missing_post/2,
        % malformed_request/2,
        % uri_too_long/2,
        % known_content_type/2,
        % valid_content_headers/2,
        % valid_entity_length/2,
        % options/2,
        % allowed_methods/2,
        % known_methods/2,
        % content_types_provided/2,
        % content_types_accepted/2,
        % delete_resource/2,
        % delete_completed/2,
        % post_is_create/2,
        % create_path/2,
        % process_post/2,
        % language_available/2,
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
        % finish_request/2,
        init/1
        ]).

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
