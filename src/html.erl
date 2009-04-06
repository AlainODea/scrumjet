-module(html).

-include("scrumjet.hrl").
-compile(export_all).

-spec li(#scrumjet_task{}|#scrumjet_category{}|#scrumjet_board{}, [iolist()]) -> iolist().
li(R=#scrumjet_task{id=ID, headline=L}, Items) -> li_("task", R, L, ID, Items);
li(R=#scrumjet_category{id=ID, name=L}, Items) -> li_("category", R, L, ID, Items);
li(R=#scrumjet_board{id=ID, title=  L}, Items) -> li_("board", R, L, ID, Items).

-spec li_(string(), #scrumjet_task{}|#scrumjet_category{}|#scrumjet_board{}, string(), string(), [iolist()]) -> iolist().
li_(Class, Record, Label, ID, Items) ->
    Href = uri:for(Record),
    [<<"
<li><a class='">>,Class,<<"' href='">>,Href,<<"'>">>,Label,<<" (">>,ID,<<")</a></li>">>|Items].
