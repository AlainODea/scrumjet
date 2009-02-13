%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.
%% @doc ScrumJet Task.

-module(scrumjet_board_resource).
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

-record(context, {board}).

-include("scrumjet.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, #context{}}.

resource_exists(ReqProps, Context) ->
    ID = list_to_binary(?PATH(ReqProps)),
    case ets:lookup(scrumjet_board, ID) of
        [] -> {false, Context};
        [Board] -> {true, Context#context{board=Board}}
    end.

to_html(_ReqProps, Context) ->
    #context{board=#scrumjet_board{id=ID, title=Title}} = Context,
    {[<<"
<html>
<head>
<title>Task ID: ",ID/binary," - ScrumJet</title>
<body>
<h1>",Title/binary," (",ID/binary,")</h1>
<p>",Title/binary,"</p>
</body>
</html>
    ">>], Context}.

%% should accept PUT requests to create new boards
allowed_methods(_ReqProps, Context) -> {['GET', 'HEAD', 'PUT'], Context}.

content_types_accepted(_ReqProps, Context) -> {[{"application/x-www-urlencoded", from_webform}], Context}.

from_webform(ReqProps, Context) ->
    ID = list_to_binary(?PATH(ReqProps)),
    Req = ?REQ(ReqProps),
    Params = Req:parse_qs(),
    Title = proplists:get_value(title, Params, <<"">>),
    ets:insert(scrumjet_board, #scrumjet_board{id=ID, title=Title}),
    {true, Context}.
