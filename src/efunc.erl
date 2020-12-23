-module(efunc).
%% API
-export([
  curry/2,
  pipe/1
]).

curry(F, Args) when is_function(F), is_list(Args) ->
  {arity, Arity} = erlang:fun_info(F, arity),
  N = Arity - erlang:length(Args),
  curry_n(N, F, Args);
curry(F, Arg) when is_function(F) ->
  curry(F, [Arg]).

curry_n(N, _F, _Args) when N < 0 ->
  exit({bad_arity, N});
curry_n(N, F, Args) when N == 0 ->
  erlang:apply(F, Args);
curry_n(N, F, Args) ->
  fun(L) when is_list(L) ->
    N2 = N - erlang:length(L),
    curry_n(N2, F, Args ++ L);
    (E) ->
      N2 = N - 1,
      curry_n(N2, F, Args ++ [E])
  end.

pipe(F) when is_function(F, 1) ->
  F;
pipe(L) when is_list(L) ->
  pipe(L, fun(X) -> X end).

pipe([], Acc) ->
  Acc;
pipe([F | L], Acc) when is_function(F, 1) ->
  Acc2 = fun(X) -> F(Acc(X)) end,
  pipe(L, Acc2).






