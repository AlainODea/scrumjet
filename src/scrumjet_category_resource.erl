%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Category Resource.

-module(scrumjet_category_resource).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([init/1]).
-export([to_html/2,
         allowed_methods/2,
    	 resource_exists/2,
         % last_modified/2,
         % content_types_provided/2,
         content_types_accepted/2,
         from_webform/2
         % delete_resource/2,
         % post_is_create/2,
         % create_path/2,
         % provide_content/2,
         % accept_content/2,
         % generate_etag/2
    	 ]).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(context, {category}).

init([]) -> {ok, #context{}}.

resource_exists(ReqProps, Context) ->
    ID = list_to_binary(?PATH(ReqProps)),
    case ets:lookup(scrumjet_category, ID) of
        [] -> {false, Context};
        [Category] -> {true, Context#context{category=Category}}
    end.

to_html(ReqProps, State) ->
    ID = list_to_binary(?PATH(ReqProps)),
    {[<<"
<html>
<head>
<title>Category ID: ",ID/binary," - ScrumJet</title>
<body>
<h1>ScrumJet Category ID: ",ID/binary,"</h1>
<h2>Tasks</h2>
<ul>
">>,
lists:foldl(fun html:li/2, [],
    qlc:eval(
        qlc:q([Task || Task <- ets:table(scrumjet_task),
            {CategoryID, TaskID} <- ets:table(scrumjet_category_tasks),
            CategoryID =:= ID,
            TaskID =:= Task#scrumjet_task.id])
    )
),
<<"
</ul>
</body>
</html>
">>], State}.

%% should accept PUT requests to create new tasks
allowed_methods(_ReqProps, Context) -> {['GET', 'HEAD', 'PUT'], Context}.

content_types_accepted(_ReqProps, Context) -> {[{"application/x-www-urlencoded", from_webform}], Context}.

from_webform(ReqProps, Context) ->
    ID = list_to_binary(?PATH(ReqProps)),
    Req = ?REQ(ReqProps),
    Params = Req:parse_qs(),
    Name = proplists:get_value(name, Params, <<"">>),
    ets:insert(scrumjet_task, #scrumjet_category{id=ID, name=Name}),
    {true, Context}.
