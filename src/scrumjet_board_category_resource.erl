%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Board-Category Relationship Resource.

-module(scrumjet_board_category_resource).
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

-record(context, {board_category}).

init([]) -> {ok, #context{}}.

resource_exists(ReqData, Context) ->
    DispatchPath = wrq:disp_path(ReqData),
    case string:tokens(DispatchPath, ";") of
        [ID, CategoryID] ->
            case scrumjet_board_category:find({id, ID, CategoryID}) of
                [] -> {false, ReqData, Context#context{board_category=#scrumjet_board_category{id=ID, category_id=CategoryID}}};
                [BoardCategory] -> {true, ReqData, Context#context{board_category=BoardCategory}}
            end;
        _ -> {{halt, 400}, ReqData, Context}
    end.

to_html(ReqData, Context=#context{board_category=#scrumjet_board_category{id=ID, category_id=CategoryID}}) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Board/Category ID: ">>,ID,<<"/">>,CategoryID,<<" - ScrumJet</title>
<body>
<a id='category' href='">>,uri:for(#scrumjet_category{id=CategoryID}),<<"'>Category ">>,CategoryID,<<"</a>
is on <a id='board' href='">>,uri:for(#scrumjet_board{id=ID}),<<"'>Board ">>,ID,<<"</a>
</body>
</html>
">>], ReqData, Context}.

allowed_methods(ReqData, Context) -> {['GET', 'HEAD', 'PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) -> {[{
    case wrq:get_req_header("content-type", ReqData) of
        undefined -> "application/octet-stream";
        X -> X
    end, from_any}], ReqData, Context}.

%% to relate a board and a category you simply assert that the relationship
%% exists by PUTting it there
from_any(ReqData, Context=#context{board_category=BoardCategory}) ->
    scrumjet_board_category:store(BoardCategory),
    {true, ReqData, Context}.
