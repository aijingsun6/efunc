-module(efunc).
%% API
-export([curry/2]).

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
  fun(L) ->
    N2 = N - erlang:length(L),
    curry_n(N2, F, Args ++ L)
  end.





