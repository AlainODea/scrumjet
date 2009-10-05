-module(scrumjet_task).

-include("scrumjet.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, store/1, find/1, range/2]).

start() ->
    try
        set = mnesia:table_info(?MODULE, type)
    catch
        exit:{aborted, {no_exists, ?MODULE, type}} ->
            {atomic, ok} = mnesia:create_table(?MODULE,
                [{attributes, record_info(fields, ?MODULE)},
                {type, set},
                {disc_copies, [node()]}])
    end.

store(Record=#?MODULE{ctime=undefined}) ->
    F = fun() ->
        Now = erlang:now(),
        mnesia:write(Record#?MODULE{ctime=Now, mtime=Now})
    end,
    mnesia:transaction(F);
store(Record=#?MODULE{}) ->
    F = fun() ->
        Now = erlang:now(),
        mnesia:write(Record#?MODULE{mtime=Now})
    end,
    mnesia:transaction(F).

range(Start, End) ->
    F = fun() ->
        mnesia_ext:limit(?MODULE, Start, End - Start)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records.

find({id, Id}) ->
    F = fun() ->
        Query = qlc:q([M || M <- mnesia:table(?MODULE),
                  M#?MODULE.id =:= Id]),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records.

