-module(uri).

-include("scrumjet.hrl").

-compile(export_all).

for(#scrumjet_task{id=ID}) -> <<"/tasks/", ID/binary>>;
for(#scrumjet_category{id=ID}) -> <<"/categories/", ID/binary>>;
for(#scrumjet_board{id=ID}) -> <<"/boards/", ID/binary>>;
for(Other) -> Other.
