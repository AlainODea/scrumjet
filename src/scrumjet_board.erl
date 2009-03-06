-module(scrumjet_board).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("scrumjet.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0, store/1, find/1, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%% Client API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Record=#?MODULE{}) ->
    gen_server:call(?SERVER, {insert, Record}).

find(Params) ->
    case gen_server:call(?SERVER, {retrieve, Params}) of
        {ok, Records} -> Records
    end.

shutdown() ->
    gen_server:call(?SERVER, stop).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting...~n", [?MODULE, self()]),
    init_store(),
    {ok, #state{}}.

handle_call({insert, Record=#?MODULE{}}, _From, State) ->
    insert(Record),
    {reply, ok, State};

handle_call({retrieve, Params}, _From, State) ->
    Records = retrieve(Params),
    {reply, {ok, Records}, State};

handle_call(stop, _From, State) ->
    mnesia:stop(),
    io:format("Shutting down...~n"),
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    {reply, ignored_message, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
insert(Record=#?MODULE{}) ->
    F = fun() ->
        Now = erlang:now(),
        mnesia:write(Record#?MODULE{ctime=Now, mtime=Now})
    end,
    mnesia:transaction(F).

retrieve({id, Id}) ->
    F = fun() ->
        Query = qlc:q([M || M <- mnesia:table(?MODULE),
                  M#?MODULE.id =:= Id]),
        qlc:eval(Query)
    end,
    {atomic, Records} = mnesia:transaction(F),
    Records.

init_store() ->
    try
        mnesia:table_info(?MODULE, type)
    catch
        exit: _ ->
            mnesia:create_table(?MODULE, [{attributes, record_info(fields, ?MODULE)},
                {type, bag},
                {disc_copies, [node()]}])
    end.

