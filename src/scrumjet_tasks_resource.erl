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
        case range(wrq:get_req_header("Range", ReqData)) of
            undefined ->
                QRange = query_range(wrq:get_qs_value("start", ReqData),
                    wrq:get_qs_value("end", ReqData)),
                invalid_range(QRange, ReqData, Context);
            Range -> invalid_range(Range, ReqData, Context)
        end
    catch _:_ ->
        {true, ReqData, Context}
    end.

invalid_range({Start, End}, ReqData, Context) when Start > End ->
    {true, ReqData, Context};
invalid_range({Start, End}, ReqData, Context) when (End - Start) > 1000 ->
    % reinterpret request as a valid smaller range
    invalid_range({Start, Start + 1000}, ReqData, Context);
invalid_range({Start, End}, ReqData, Context) ->
    Count = mnesia:table_info(scrumjet_task, size),
    ContentRange = lists:flatten(io_lib:format(
        "items ~w-~w/~w", [Start, End, Count])),
    NewReqData = wrq:set_resp_header("Content-Range", ContentRange, ReqData),
    {false, NewReqData, Context#context{range= {Start,End} }};
invalid_range(undefined, ReqData, Context) ->
    % default to 1000 items or whole data set
    Count = mnesia:table_info(scrumjet_task, size),
    invalid_range({0,lists:max([1000, Count])}, ReqData, Context).

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}, {"application/json", to_json}],
        ReqData, Context}.

to_html(ReqData, Context=#context{range={Start, End}}) ->
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
lists:foldl(fun html:li/2, [], tasks(Start, End)),
<<"
</ul>">>,
pager_links(Start, End),
<<"
</body>
</html>
">>], ReqData, Context}.

pager_links(Start, End) ->
    PageSize = End - Start,
    [pager_link("Previous", Start - PageSize, End - PageSize),
    pager_link("Next", Start + PageSize, End + PageSize)].

pager_link(Label, Start, End) ->
    io_lib:format("<a href='./?start=~w&end=~w'>~s</a>", [Start, End, Label]).

to_json(ReqData, Context=#context{range={Start, End}}) ->
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
    scrumjet_datastore:range(scrumjet_task, Start, End).

-spec query_range(string(), string()) -> {integer(), integer()}.
query_range(undefined, _) -> undefined;
query_range(_, undefined) -> undefined;
query_range(StartQ, EndQ) ->
    {Start, []} = string:to_integer(StartQ),
    {End, []} = string:to_integer(EndQ),
    {Start, End}.
