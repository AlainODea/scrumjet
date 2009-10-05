% Author: Paul Mineiro
% SOURCE: http://erlanganswers.com/web/mcedemo/mnesia/OrderedBy.html
-module (mnesia_ext).
-export ([ limit/3 ]).

limit (Tab, Offset, Number) ->
  seek (Offset,
        Number,
        mnesia:select (Tab,
                       [ { mnesia:table_info (Tab, wild_pattern), [], [ '$_' ] } ],
                       10,
                       read)).

seek (_Offset, _Number, '$end_of_table') ->
  [];
seek (Offset, Number, X) when Offset =< 0 ->
  read (Number, X, []);
seek (Offset, Number, { Results, Cont }) ->
  NumResults = length (Results),
  case Offset > NumResults of
    true ->
      seek (Offset - NumResults, Number, mnesia:select (Cont));
    false ->
      { _, DontDrop } = lists:split (Offset, Results),
      Keep = lists:sublist (DontDrop, Number),
      read (Number - length (Keep), mnesia:select (Cont), [ Keep ])
  end.

read (Number, _, Acc) when Number =< 0 ->
  lists:foldl (fun erlang:'++'/2, [], Acc);
read (_Number, '$end_of_table', Acc) ->
  lists:foldl (fun erlang:'++'/2, [], Acc);
read (Number, { Results, Cont }, Acc) ->
  NumResults = length (Results),
  case Number > NumResults of
    true ->
      read (Number - NumResults, mnesia:select (Cont), [ Results | Acc ]);
    false ->
      { Keep, _ } = lists:split (Number, Results),
      lists:foldl (fun erlang:'++'/2, Keep, Acc)
  end.