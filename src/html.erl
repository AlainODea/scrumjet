-module(html).

-include("scrumjet.hrl").
-export([li/2, to_entities/1, head/0]).

-spec li(sj_record(), [iolist()]) -> iolist().
li(R=#scrumjet_task{id=ID, headline=L}, Items) -> li_("task", R, L, ID, Items);
li(R=#scrumjet_category{id=ID, name=L}, Items) -> li_("category", R, L, ID, Items);
li(R=#scrumjet_board{id=ID, title=  L}, Items) -> li_("board", R, L, ID, Items).

-spec li_(string(), #scrumjet_task{}|#scrumjet_category{}|#scrumjet_board{}, string(), string(), [iolist()]) -> iolist().
li_(Class, Record, Label, ID, Items) ->
    Href = uri:for(Record),
    [<<"
<li><a class='">>,Class,<<"' href='">>,Href,<<"'>">>,Label,<<" (">>,ID,<<")</a></li>">>|Items].

% XSS Prevention Cheat Sheet http://www.owasp.org/index.php/XSS_%28Cross_Site_Scripting%29_Prevention_Cheat_Sheet#Why_Can.27t_I_Just_HTML_Entity_Encode_Untrusted_Data.3F
% & --> &amp;
% < --> &lt;
% > --> &gt;
% " --> &quot;
% ' --> &#x27;     &apos; is not recommended
% / --> &#x2F;     forward slash is included as it helps end an HTML entity
to_entities([$&|Xml]) -> ["&gt;"|to_entities(Xml)];
to_entities([$<|Xml]) -> ["&lt;"|to_entities(Xml)];
to_entities([$>|Xml]) -> ["&gt;"|to_entities(Xml)];
to_entities([$"|Xml]) -> ["&quot;"|to_entities(Xml)];
to_entities([$'|Xml]) -> ["&#x27;"|to_entities(Xml)];
to_entities([$/|Xml]) -> ["&#x2F;"|to_entities(Xml)];
to_entities([Char|Xml]) -> [Char|to_entities(Xml)].

head() ->
    <<"
<link rel='stylesheet' type='text/css' href='http://ajax.googleapis.com/ajax/libs/dojo/1.3.2/dijit/themes/tundra/tundra.css'>
<style type='text/css'>
@import url('http://ajax.googleapis.com/ajax/libs/dojo/1.3.2/dojox/grid/resources/Grid.css');
@import url('http://ajax.googleapis.com/ajax/libs/dojo/1.3.2/dojox/grid/resources/tundraGrid.css');
</style>
<script type='text/javascript' src='http://ajax.googleapis.com/ajax/libs/dojo/1.3.2/dojo/dojo.xd.js'></script>
">>.
