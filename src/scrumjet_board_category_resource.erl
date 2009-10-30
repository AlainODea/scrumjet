%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Board-Category Relationship Resource.

-module(scrumjet_board_category_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1]).
%% resource functions
-export([resource_exists/2,
         allowed_methods/2,
         content_types_accepted/2]).
%% representation handlers
-export([to_html/2]).
%% representation parsers
-export([from_any/2]).

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
                [] ->
                    {false, ReqData, Context#context{board_category=
                    #scrumjet_board_category{id=ID, category_id=CategoryID}}};
                [BoardCategory] ->
                    {true, ReqData, Context#context{
                    board_category=BoardCategory}}
            end;
        _ -> {{halt, 400}, ReqData, Context}
    end.

to_html(ReqData, Context=#context{board_category=
        #scrumjet_board_category{id=ID, category_id=CategoryID}}) ->
    {[<<"<!DOCTYPE html><html><head>"
"<title>Board/Category ID: ">>,ID,<<"/">>,CategoryID,<<" - ScrumJet</title>"
"<body>"
"<a id='category' href='">>,
scrumjet_uri:for(#scrumjet_category{id=CategoryID}),<<"'>Category ">>,CategoryID,<<"</a>"
"is on <a id='board' href='">>,scrumjet_uri:for(#scrumjet_board{id=ID}),
<<"'>Board ">>,ID,<<"</a></body></html>">>], ReqData, Context}.

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
