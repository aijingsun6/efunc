-module(efunc).
%% API
-export([
  curry/1,
  curry/2,
  pipe/1
]).

-export([
  prop/1,
  prop_eq/2
]).

%% warp list
-export([
  all/1,
  any/1,
  map/1,
  filter/1
]).

-export([
  add/1,
  adjust/2
]).

curry(F) ->
  curry(F, []).

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

-spec prop(K :: any()) -> fun((X :: any())-> Y :: any()).
prop(K) ->
  fun(M) when is_map(M) -> maps:get(K, M);(L) when is_list(L) -> proplists:get_value(K, L) end.

prop_pipe(K, L) when is_list(L) ->
  pipe([prop(K) | L]);
prop_pipe(K, E) ->
  pipe([prop(K), E]).

-spec prop_eq(K :: any(), V :: any()) -> fun((X :: any()) -> boolean()).
prop_eq(K, V) ->
  prop_pipe(K, fun(X) -> X =:= V end).

all(F) ->
  fun(L) -> lists:all(F, L) end.

any(F) ->
  fun(L) -> lists:any(F, L) end.

map(F) ->
  fun(L) when is_list(L) -> lists:map(F, L);(E) -> F(E) end.

filter(F) ->
  fun(L) when is_list(L) -> lists:filter(F, L) end.

add(X) ->
  fun(A) -> A + X end.

adjust(IDX, F) ->
  fun(L) -> adjust_i(L, 1, F, IDX, []) end.

adjust_i([], _N, _F, _T, Acc) ->
  lists:reverse(Acc);
adjust_i([E | L], N, F, T, Acc) when N =:= T ->
  adjust_i(L, N + 1, F, T, [F(E) | Acc]);
adjust_i([E | L], N, F, T, Acc) ->
  adjust_i(L, N + 1, F, T, [E | Acc]).






