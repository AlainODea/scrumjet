%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_task_resource).
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

-record(context, {task}).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, #context{}}.

resource_exists(ReqProps, Context) ->
    ID = ?PATH(ReqProps),
    case scrumjet_task:find({id, ID}) of
        [] -> {false, Context};
        [Task] -> {true, Context#context{task=Task}}
    end.

to_html(_ReqProps, Context=#context{task=#scrumjet_task{id=ID, headline=Headline, description=Description}}) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Task ID: ">>,ID,<<" - ScrumJet</title>
<body>
<h1>",Headline/binary," (">>,ID,<<")</h1>
<p>",Description/binary,"</p>
</body>
</html>
">>], Context}.

%% should accept PUT requests to create new tasks
allowed_methods(_ReqProps, Context) -> {['GET', 'HEAD', 'PUT'], Context}.

content_types_accepted(_ReqProps, Context) -> {[{"application/x-www-urlencoded", from_webform}], Context}.

from_webform(ReqProps, Context) ->
    ID = ?PATH(ReqProps),
    Req = ?REQ(ReqProps),
    Body = Req:recv_body(),
    Params = mochiweb_util:parse_qs(Body),
    Headline = proplists:get_value("headline", Params, ""),
    Description = proplists:get_value("description", Params, ""),
    scrumjet_task:store(#scrumjet_task{id=ID, description=list_to_binary(Description), headline=list_to_binary(Headline)}),
    {true, Context}.
