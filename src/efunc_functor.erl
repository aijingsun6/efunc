-module(efunc_functor).

%% API
-export([
  new/1,
  value/1,
  map/2,
  pipe/2
]).

-record(functor, {value}).

new(V) ->
  #functor{value = V}.

value(#functor{value = V}) ->
  V.

map(#functor{value = V}, F) when is_function(F, 1) ->
  #functor{value = F(V)}.

pipe(#functor{} = V, []) ->
  V;
pipe(#functor{value = V}, [F | L]) ->
  pipe(#functor{value = F(V)}, L).