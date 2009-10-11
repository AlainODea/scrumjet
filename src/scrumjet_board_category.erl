%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Board-Category Relationship Storage Server.

-module(scrumjet_board_category).

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

find({id, Id, CategoryId}) ->
    F = fun() ->
        Query = qlc:q([M || M <- mnesia:table(?MODULE),
                  M#?MODULE.id =:= Id,
                  M#?MODULE.category_id =:= CategoryId]),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records;
find({categories, Id}) ->
    F = fun() ->
        Query = qlc:q([Category ||
                    Join <- mnesia:table(?MODULE),
                    Category <- mnesia:table(scrumjet_category),
                    Join#?MODULE.id =:= Id,
                    Category#scrumjet_task.id =:= Join#?MODULE.category_id]),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records;
find({boards, Id}) ->
    F = fun() ->
        Query = qlc:q([Board ||
                    Join <- mnesia:table(?MODULE),
                    Board <- mnesia:table(scrumjet_board),
                    Join#?MODULE.category_id =:= Id,
                    Board#scrumjet_task.id =:= Join#?MODULE.id]),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records.

