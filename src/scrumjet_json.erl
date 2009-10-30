-module(scrumjet_json).

-export([value/1]).

-include("scrumjet.hrl").

value(Bin) when is_binary(Bin) -> Bin;
value(Atom) when is_atom(Atom) -> Atom;
value(List) when is_list(List) -> array(List);
value(Other) -> object(Other).

array(List) when is_list(List) -> [value(V) || V <- List].

object(#scrumjet_task{id=ID, headline=H, description=D}) ->
    {struct, [
        {class, task},
        {id, binary(ID)},
        {headline, binary(H)},
        {description, binary(D)}
    ]}.

binary(Bin) when is_binary(Bin) -> Bin;
binary(List) when is_list(List) -> list_to_binary(List).
