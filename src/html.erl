-module(html).

-include("scrumjet.hrl").
-compile(export_all).

list_item(#scrumjet_task{id=ID, headline=Headline}, Tasks) ->
    [<<"
<li><a href=\"",ID/binary,"\">",Headline/binary," (",ID/binary,")</a></li>">>|Tasks];
list_item(#scrumjet_category{id=ID, name=Name}, Tasks) ->
    [<<"
<li><a href=\"",ID/binary,"\">",Name/binary," (",ID/binary,")</a></li>">>|Tasks];
list_item(#scrumjet_board{id=ID, title=Title}, Tasks) ->
    [<<"
<li><a href=\"",ID/binary,"\">",Title/binary," (",ID/binary,")</a></li>">>|Tasks];
list_item(Other, Tasks) ->
    [<<"
<li>",Other/binary,"</li>">>|Tasks].
