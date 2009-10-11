%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_board_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1]).
%% resource functions
-export([allowed_methods/2,
         resource_exists/2,
         content_types_accepted/2]).
%% representation generators
-export([to_html/2]).
%% representation parsers
-export([from_webform/2]).

-record(context, {board}).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, #context{}}.

resource_exists(ReqData, Context) ->
    ID = wrq:disp_path(ReqData),
    case scrumjet_datastore:find(scrumjet_board, {id, ID}) of
        [] -> {false, ReqData, Context#context{board=#scrumjet_board{id=ID}}};
        [Board] -> {true, ReqData, Context#context{board=Board}}
    end.

to_html(ReqData, #context{board=#scrumjet_board{id=ID, title=Title}}=Context) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Board ID: ">>,ID,<<" - ScrumJet</title>
<body>
<h1>">>,Title,<<" (">>,ID,<<")</h1>
<h2>Categories</h2>
<ul id='categories'>">>,
categories(ID),
<<"
</ul>
</body>
</html>
">>], ReqData, Context}.

%% should accept PUT requests to create new boards
allowed_methods(ReqData, Context) -> {['GET', 'HEAD', 'PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/x-www-urlencoded", from_webform}], ReqData, Context}.

from_webform(ReqData, Context=#context{board=Board}) ->
    Params = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    Title = proplists:get_value("title", Params, ""),
    scrumjet_datastore:store(Board#scrumjet_board{title=Title}),
    {true, ReqData, Context}.

-spec categories(string()) -> iolist().
categories(ID) ->
    lists:foldl(fun html:li/2, [],
        scrumjet_board_category:find({categories, ID})).
