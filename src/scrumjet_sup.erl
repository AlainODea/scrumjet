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
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    Dispatch = [
    {[], scrumjet_resource, []},
    {["boards"], scrumjet_boards_resource, []},
    {["boards",'*'], scrumjet_board_resource, []},
    {["categories"], scrumjet_categories_resource, []},
    {["categories", '*'], scrumjet_category_resource, []},
    {["category_tasks", '*'], scrumjet_category_task_resource, []},
    {["tasks"], scrumjet_tasks_resource, []},
    {["tasks", '*'], scrumjet_task_resource, []},
    {["board_categories", '*'], scrumjet_board_category_resource, []},
    {["static", '*'], static_resource, [{root, "priv/www"}]}
    ],
    WebConfig = [
        {ip, Ip},
        {port, 8000},
            {log_dir, "priv/log"},
        {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
        {webmachine_mochiweb, start, [WebConfig]},
        permanent, 5000, worker, dynamic},
    Stores = [{S,{S,start_link,[]},permanent,5000,worker,[S]}
             || S <- [scrumjet_category_task, scrumjet_board_category]],
    Processes = [Web|Stores],
    {ok, {{one_for_one, 10, 10}, Processes}}.
