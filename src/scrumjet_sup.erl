%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.

%% @doc Supervisor for the scrumjet application.

-module(scrumjet_sup).
-author('Alain O\'Dea <alain.odea@gmail.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,   
    Dispatch = [
    {[], scrumjet_resource, []},
    {["boards"], scrumjet_boards_resource, []},
    {["boards",'*'], scrumjet_board_resource, []},
    {["categories"], scrumjet_categories_resource, []},
    {["categories", '*'], scrumjet_category_resource, []},
    {["tasks"], scrumjet_tasks_resource, []},
    {["tasks", '*'], scrumjet_task_resource, []}
    ],
    WebConfig = [
		{ip, Ip},
		{port, 8000},
            {log_dir, "priv/log"},
		{dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
	    {webmachine_mochiweb, start, [WebConfig]},
	    permanent, 5000, worker, dynamic},
	TaskStore = {scrumjet_task,
   	    {scrumjet_task, start_link, []},
   	    permanent, 5000, worker, [scrumjet_task]},
    CategoryStore = {scrumjet_category,
  	    {scrumjet_category, start_link, []},
  	    permanent, 5000, worker, [scrumjet_category]},
    BoardStore = {scrumjet_board,
 	    {scrumjet_board, start_link, []},
 	    permanent, 5000, worker, [scrumjet_board]},
    Processes = [Web, TaskStore, CategoryStore, BoardStore],
    {ok, {{one_for_one, 10, 10}, Processes}}.
