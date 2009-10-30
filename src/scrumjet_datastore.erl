-module (scrumjet_datastore).

-include("scrumjet.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, store/1, find/1, range/3]).

-define (TYPES, [scrumjet_task, scrumjet_category, scrumjet_board]).

%% Internal Helper Functions

fields(scrumjet_task) -> record_info(fields, scrumjet_task);
fields(scrumjet_category) -> record_info(fields, scrumjet_category);
fields(scrumjet_board) -> record_info(fields, scrumjet_board).

prestore(Record=#scrumjet_task{ctime=undefined}) ->
    Now = erlang:now(),
    Record#scrumjet_task{ctime=Now, mtime=Now};
prestore(Record=#scrumjet_category{ctime=undefined}) ->
    Now = erlang:now(),
    Record#scrumjet_category{ctime=Now, mtime=Now};
prestore(Record=#scrumjet_board{ctime=undefined}) ->
    Now = erlang:now(),
    Record#scrumjet_board{ctime=Now, mtime=Now};
prestore(Record=#scrumjet_task{}) ->
    Now = erlang:now(),
    Record#scrumjet_task{mtime=Now};
prestore(Record=#scrumjet_category{}) ->
    Now = erlang:now(),
    Record#scrumjet_category{mtime=Now};
prestore(Record=#scrumjet_board{}) ->
    Now = erlang:now(),
    Record#scrumjet_board{mtime=Now}.

select(#scrumjet_task{id=Id}) ->
    qlc:q([M || M <- mnesia:table(scrumjet_task),
                M#scrumjet_task.id =:= Id]);
select(#scrumjet_category{id=Id}) ->
    qlc:q([M || M <- mnesia:table(scrumjet_category),
                M#scrumjet_category.id =:= Id]);
select(#scrumjet_board{id=Id}) ->
    qlc:q([M || M <- mnesia:table(scrumjet_board),
                M#scrumjet_board.id =:= Id]).

create_table(Type) ->
    try
        set = mnesia:table_info(Type, type)
    catch
        exit:{aborted, {no_exists, Type, type}} ->
            {atomic, ok} = mnesia:create_table(Type,
                [{attributes, fields(Type)},
                {type, set},
                {disc_copies, [node()]}])
    end.

%% Public API Functions

start() -> [create_table(Type) || Type <- ?TYPES].

store(Record) ->
    F = fun() ->
        mnesia:write(prestore(Record))
    end,
    mnesia:transaction(F).

find(Params) ->
    F = fun() ->
        Query = select(Params),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records.

range(Type, Start, End) ->
    F = fun() ->
        scrumjet_mnesia:limit(Type, Start, End - Start)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records.
