-module(uri).

-include("scrumjet.hrl").

-compile(export_all).

for(#scrumjet_task{id=ID}) -> [<<"/tasks/">>, ID];
for(#scrumjet_category{id=ID}) -> [<<"/categories/">>, ID];
for(#scrumjet_board{id=ID}) -> [<<"/boards/">>, ID];
for(Other) -> Other.
