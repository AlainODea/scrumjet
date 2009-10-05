%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_tasks_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').

%% webmachine resource functions
-export([
    init/1,
    content_types_provided/2,
    malformed_request/2
    ]).

%% content generators
-export([
    to_html/2,
    to_json/2
    ]).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {
    range
}).

init([]) -> {ok, #context{}}.

malformed_request(ReqData, Context) ->
    try
        Range = range(wrq:get_req_header("Range", ReqData)),
        valid_range(Range, ReqData, Context)
    catch
        _:_ -> {true, ReqData, Context}
    end.

valid_range({Start, End}, ReqData, Context) when Start > End ->
    {false, ReqData, Context};
valid_range({Start, End}, ReqData, Context) when (End - Start) > 1000 ->
    % reinterpret request as a valid smaller range
    valid_range({Start, Start + 1000}, ReqData, Context);
valid_range({Start, End}, ReqData, Context) ->
    Count = mnesia:table_info(scrumjet_task, size),
    ContentRange = lists:flatten(io_lib:format(
        "items ~w-~w/~w", [Start, End, Count])),
    NewReqData = wrq:set_resp_header("Content-Range", ContentRange, ReqData),
    {false, NewReqData, Context#context{range= {Start,End} }};
valid_range(undefined, ReqData, Context) ->
    % default to 1000 items or whole data set
    Count = mnesia:table_info(scrumjet_task, size),
    valid_range({0,lists:max([1000, Count])}, ReqData, Context).

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}, {"application/json", to_json}],
        ReqData, Context}.

to_html(ReqData, Context) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Tasks - ScrumJet</title>">>,
html:head(),
<<"<script type='text/javascript' src='/static/tasks.js'></script>
</head>
<body style='display:none'>
<h1>ScrumJet Tasks</h1>
<ul id='tasks'>">>,
lists:foldl(fun html:li/2, [], tasks(0, 10)),
<<"
</ul>
</body>
</html>
">>], ReqData, Context}.

to_json(ReqData, Context=#context{range=undefined}) ->
    to_json_handler({0, 1000}, ReqData, Context);
to_json(ReqData, Context=#context{range={Start, End}}) ->
    to_json_handler({Start, End}, ReqData, Context).

to_json_handler({Start, End}, ReqData, Context) ->
    {mochijson2:encode([json:value(T) || T <- tasks(Start, End)]),
        ReqData, Context}.

-spec range(list()) -> {integer(), integer()}.
range("items=" ++ Range) ->
    [{Start,[]},{End,[]}] = [string:to_integer(binary_to_list(Tok)) ||
        Tok <- re:split(Range, "-")],
    {Start, End};
range(undefined) -> undefined.

-spec tasks(integer(), integer) -> [sj_record()].
tasks(Start, End) ->
    scrumjet_task:range(Start, End).

