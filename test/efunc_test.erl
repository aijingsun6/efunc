-module(efunc_test).
-author('alking').

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

curry_test() ->
  F = fun(X, Y) -> X + Y end,
  Add = efunc:curry(F, 1),
  ?assertEqual(3, Add([2])).

curry_3_test() ->
  F = fun(A1, A2, A3) -> A1 + A2 + A3 end,
  F1 = efunc:curry(F, []),
  ?assertEqual(6, F1([1, 2, 3])),
  F2 = F1([1]),
  ?assertEqual(6, F2([2, 3])),
  F3 = F2([2]),
  ?assertEqual(6, F3([3])).
