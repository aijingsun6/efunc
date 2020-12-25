-module(efunc_test).
-author('alking').
-include("efunc.hrl").
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

curry_test() ->
  F = fun(X, Y) -> X + Y end,
  Add = efunc:curry(F, 1),
  ?assertEqual(3, Add([2])).

curry_2_test() ->
  F = fun(X, Y) -> X + Y end,
  F1 = ?CURRY(F),
  F2 = F1(1),
  ?assertEqual(3, F2(2)).

curry_3_test() ->
  F = fun(A1, A2, A3) -> A1 + A2 + A3 end,
  F1 = efunc:curry(F, []),
  ?assertEqual(6, F1([1, 2, 3])),
  F2 = F1(1),
  ?assertEqual(6, F2([2, 3])),
  F3 = F2(2),
  ?assertEqual(6, F3([3])).

pipe_test() ->
  X2 = fun(X) -> X * 2 end,
  X3 = fun(X) -> X * 3 end,
  F = efunc:pipe([X2, X3]),
  {arity, 1} = erlang:fun_info(F, arity),
  ?assertEqual(6, F(1)).

prop_test() ->
  F = efunc:prop(foo),
  {arity, 1} = erlang:fun_info(F, arity),
  ?assertEqual(bar, F(#{foo => bar})).

prop_eq_test() ->
  F = efunc:prop_eq(foo, bar),
  {arity, 1} = erlang:fun_info(F, arity),
  ?assertEqual(true, F(#{foo => bar})).

all_test() ->
  F = efunc:all(fun(X) -> X == 1 end),
  ?assert(F([1, 1, 1])),
  ?assert(not F([1, 2, 1])).

count_by_test() ->
  F = efunc:count_by(fun(X) -> X > 5 end),
  L = lists:seq(1, 10),
  C = F(L),
  ?assertEqual(5, C).

sort_test() ->
  F = efunc:sort(efunc:descend()),
  L2 = F(lists:seq(1, 5)),
  ?assertEqual([5, 4, 3, 2, 1], L2).

flip_test() ->
  F = efunc:flip(),
  [2, 1, 3] = F([1, 2, 3]).

thunkify_test() ->
  F = fun(X, Y) -> X + Y end,
  F2 = efunc:thunkify(F),
  {arity, 2} = erlang:fun_info(F2, arity),
  F3 = F2(1, 2),
  {arity, 0} = erlang:fun_info(F3, arity),
  3 = F3().

cond_test() ->
  L = [
    {fun(X) -> X < 1 end, fun(_) -> 0 end},
    {fun(X) -> X >= 1 orelse X =< 10 end, fun(X) -> X * X * X end},
    {fun(X) -> X > 10 end, fun(X) -> X * X end}
  ],
  F = efunc:condition(L),
  0 = F(-1).

group_by_test() ->
  F = fun(S) when S >= 90 -> <<"A">>;
    (S) when S >= 80 -> <<"B">>;
    (S) when S >= 70 -> <<"C">>;
    (S) when S >= 60 -> <<"D">>;
    (_) -> <<"E">> end,

  F2 = efunc:group_by(F),

  M = F2(lists:seq(50, 100)),
  #{<<"E">> := E, <<"D">> := D, <<"C">> := C, <<"B">> := B, <<"A">> := A} = M,
  E = lists:seq(50, 59),
  D = lists:seq(60, 69),
  C = lists:seq(70, 79),
  B = lists:seq(80, 89),
  A = lists:seq(90, 100).

group_with_test() ->
  SF = fun(S) when S >= 90 -> <<"A">>;
    (S) when S >= 80 -> <<"B">>;
    (S) when S >= 70 -> <<"C">>;
    (S) when S >= 60 -> <<"D">>;
    (_) -> <<"E">> end,
  F = efunc:group_with(
    fun(X, Y) -> SF(X) =:= SF(Y) end
  ),
  L = F(lists:seq(50, 100)),
  [E, D, C, B, A] = L,
  E = lists:seq(50, 59),
  D = lists:seq(60, 69),
  C = lists:seq(70, 79),
  B = lists:seq(80, 89),
  A = lists:seq(90, 100).

