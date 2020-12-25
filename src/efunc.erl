-module(efunc).
%% API
-export([
  curry/1,
  curry/2,
  pipe/1,
  compose/1
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
  filter/1,
  foreach/1,
  foldl/2,
  foldr/2
]).

-export([
  add/1,
  adjust/2,
  always/1,
  and_then/1,
  ascend/0,
  ascend/1,
  descend/0,
  descend/1,
  sort/1,
  count_by/1,
  empty/0,
  flip/0,
  reverse/0,
  tap/1,
  thunkify/1,
  condition/1,
  equals/1
]).

-define(LAZY(S, Lazy), case Lazy of true -> fun() -> S end;false -> S end).

arity(N, F, L, Lazy) when N == 0 -> fun() -> ?LAZY(erlang:apply(F, L), Lazy) end;
arity(N, F, L, Lazy) when N == 1 -> fun(X) -> ?LAZY(erlang:apply(F, [X | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 2 -> fun(A1, A2) -> ?LAZY(erlang:apply(F, [A1, A2 | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 3 -> fun(A1, A2, A3) -> ?LAZY(erlang:apply(F, [A1, A2, A3 | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 4 -> fun(A1, A2, A3, A4) -> ?LAZY(erlang:apply(F, [A1, A2, A3, A4 | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 5 ->
  fun(A1, A2, A3, A4, A5) -> ?LAZY(erlang:apply(F, [A1, A2, A3, A4, A5 | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 6 ->
  fun(A1, A2, A3, A4, A5, A6) -> ?LAZY(erlang:apply(F, [A1, A2, A3, A4, A5, A6 | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 7 ->
  fun(A1, A2, A3, A4, A5, A6, A7) -> ?LAZY(erlang:apply(F, [A1, A2, A3, A4, A5, A6, A7 | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 8 ->
  fun(A1, A2, A3, A4, A5, A6, A7, A8) -> ?LAZY(erlang:apply(F, [A1, A2, A3, A4, A5, A6, A7, A8 | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 9 ->
  fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> ?LAZY(erlang:apply(F, [A1, A2, A3, A4, A5, A6, A7, A8, A9 | L]), Lazy) end;
arity(N, F, L, Lazy) when N == 10 ->
  fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ->
    ?LAZY(erlang:apply(F, [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10 | L]), Lazy) end;
arity(N, _F, _L, _Lazy) when N > 10 -> exit({bad_arity, N}).

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

compose(F) when is_function(F) -> F;
compose(L) when is_list(L) -> pipe(lists:reverse(L)).

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

foreach(F) ->
  fun(L) -> lists:foreach(F, L), L end.

foldl(F, Acc) ->
  fun(L) -> lists:foldl(F, Acc, L) end.

foldr(F, Acc) ->
  fun(L) -> lists:foldr(F, Acc, L) end.

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

-spec always(T :: any()) -> fun(() -> T :: any()).
always(T) ->
  fun() -> T end.

and_then(F) ->
  fun(X) ->
    case X of
      {ok, E} -> {ok, F(E)};
      Err -> Err
    end
  end.

ascend() ->
  fun(A, B) -> A < B end.

ascend(F) ->
  fun(A, B) -> F(A) < F(B) end.

count_by(F) ->
  fun(L) when is_list(L) -> erlang:length(lists:filter(F, L)) end.

descend() -> fun(A, B) -> B < A end.

descend(F) -> fun(A, B) -> F(B) < F(A) end.

sort(F) -> fun(L) -> lists:sort(F, L) end.

empty() ->
  fun(L) when is_list(L) -> [];
    (M) when is_map(M) -> #{};
    (Bin) when is_binary(Bin) -> <<>>;
    (T) when is_tuple(T) -> {} end.

flip() ->
  fun([]) -> [];
    ([T]) -> [T];
    ([A, B | L]) -> [B, A | L] end.

reverse() ->
  fun(L) -> lists:reverse(L) end.

%% Runs the given function with the supplied object, then returns the object
tap(F) ->
  fun(X) -> F(X), X end.

%% Creates a thunk out of a function. A thunk delays a calculation until its result is needed, providing lazy evaluation of arguments.
thunkify(F) ->
  {arity, Arity} = erlang:fun_info(F, arity),
  arity(Arity, F, [], true).

condition(L) ->
  fun(X) -> condition(L, X) end.

condition([], X) ->
  X;
condition([{Pre, F} | L], X) ->
  case Pre(X) of
    true -> F(X);
    false -> condition(L, X)
  end.

equals(F) ->
  fun(X) -> F(X) end.
