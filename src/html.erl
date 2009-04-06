-module(html).

-include("scrumjet.hrl").
-compile(export_all).

-spec li(#scrumjet_task{}|#scrumjet_category{}|#scrumjet_board{}, [iolist()]) -> iolist().
li(R=#scrumjet_task{id=ID, headline=L}, Items) -> li_(R, L, ID, Items);
li(R=#scrumjet_category{id=ID, name=L}, Items) -> li_(R, L, ID, Items);
li(R=#scrumjet_board{id=ID, title=  L}, Items) -> li_(R, L, ID, Items).

-spec li_(#scrumjet_task{}|#scrumjet_category{}|#scrumjet_board{}, string(), string(), [iolist()]) -> iolist().
li_(Record, Label, ID, Items) ->
    Href = uri:for(Record),
    [<<"
<li><a href='">>,Href,<<"'>">>,Label,<<" (">>,ID,<<")</a></li>">>|Items].
