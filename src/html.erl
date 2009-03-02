-module(html).

-include("scrumjet.hrl").
-compile(export_all).

li(R=#scrumjet_task{id=ID, headline=L}, Items) -> li_(R, L, ID, Items);
li(R=#scrumjet_category{id=ID, name=L}, Items) -> li_(R, L, ID, Items);
li(R=#scrumjet_board{id=ID, title=  L}, Items) -> li_(R, L, ID, Items);
li(Other, Items) ->
    [<<"
<li>",Other/binary,"</li>">>|Items].

li_(Record, Label, ID, Items) ->
    Href = uri:for(Record),
    [<<"
<li><a href='",Href/binary,"'>",Label/binary," (",ID/binary,")</a></li>">>|Items].
