%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Category storage server.

-module(scrumjet_category).

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
    init_store(),
    {ok, #state{}}.

handle_call({insert, Record=#?MODULE{}}, _From, Context) ->
    insert(Record),
    {reply, ok, Context};

handle_call({retrieve, Params}, _From, Context) ->
    Records = retrieve(Params),
    {reply, {ok, Records}, Context};

handle_call(stop, _From, Context) ->
    mnesia:stop(),
    {stop, normal, Context};

handle_call(_Request, _From, Context) ->
    {reply, ignored_message, Context}.

handle_cast(_Msg, Context) ->
    {noreply, Context}.

handle_info(_Info, Context) ->
    {noreply, Context}.

terminate(_Reason, _Context) ->
    ok.

code_change(_OldVsn, Context, _Extra) ->
    {ok, Context}.

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
    mnesia:start(),
    try
        set = mnesia:table_info(?MODULE, type)
    catch
        exit:{aborted, {no_exists, ?MODULE, type}} ->
            {atomic, ok} = mnesia:create_table(?MODULE,
                [{attributes, record_info(fields, ?MODULE)},
                {type, set},
                {disc_copies, [node()]}])
    end.

