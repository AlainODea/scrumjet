%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Category-Task relationship storage server.

-module(scrumjet_category_task).

-define(SERVER, ?MODULE).

-include("scrumjet.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, store/1, find/1]).

start() ->
    mnesia:start(),
    try
        bag = mnesia:table_info(?MODULE, type)
    catch
        exit:{aborted, {no_exists, ?MODULE, type}} ->
            {atomic, ok} = mnesia:create_table(?MODULE,
                [{attributes, record_info(fields, ?MODULE)},
                {type, bag},
                {disc_copies, [node()]}])
    end.

store(Record=#?MODULE{}) ->
    F = fun() ->
        mnesia:write(Record)
    end,
    mnesia:transaction(F).

find({id, Id, TaskId}) ->
    F = fun() ->
        Query = qlc:q([M || M <- mnesia:table(?MODULE),
                  M#?MODULE.id =:= Id,
                  M#?MODULE.task_id =:= TaskId]),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records;
find({tasks, Id}) ->
    F = fun() ->
        Query = qlc:q([Task ||
                    Join <- mnesia:table(?MODULE),
                    Task <- mnesia:table(scrumjet_task),
                    Join#?MODULE.id =:= Id,
                    Task#scrumjet_task.id =:= Join#?MODULE.task_id]),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records;
find({categories, Id}) ->
    F = fun() ->
        Query = qlc:q([Category ||
                    Join <- mnesia:table(?MODULE),
                    Category <- mnesia:table(scrumjet_category),
                    Join#?MODULE.task_id =:= Id,
                    Category#scrumjet_task.id =:= Join#?MODULE.id]),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records.
