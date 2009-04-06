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

resource_exists(ReqData, Context) ->
    ID = wrq:disp_path(ReqData),
    case scrumjet_task:find({id, ID}) of
        [] -> {false, ReqData, Context#context{task=#scrumjet_task{id=ID}}};
        [Task] -> {true, ReqData, Context#context{task=Task}}
    end.

to_html(ReqData, Context=#context{task=#scrumjet_task{id=ID, headline=Headline, description=Description}}) ->
    {[<<"<!DOCTYPE html>
<html>
<head>
<title>Task ID: ">>,ID,<<" - ScrumJet</title>
<body>
<h1>">>,Headline,<<" (">>,ID,<<")</h1>
<p>">>,Description,<<"</p>
<h2>Categories</h2>
<ul id='categories'>
">>,
categories(ID),
<<"
</ul>
</body>
</html>
">>], ReqData, Context}.

%% should accept PUT requests to create new tasks
allowed_methods(ReqData, Context) -> {['GET', 'HEAD', 'PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) -> {[{"application/x-www-urlencoded", from_webform}], ReqData, Context}.

from_webform(ReqData, Context=#context{task=Task}) ->
    Params = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    Headline = proplists:get_value("headline", Params, ""),
    Description = proplists:get_value("description", Params, ""),
    scrumjet_task:store(Task#scrumjet_task{description=Description, headline=Headline}),
    {true, ReqData, Context}.

-spec categories(string()) -> iolist().
categories(ID) -> lists:foldl(fun html:li/2, [], scrumjet_category_task:find({categories, ID})).