-module(econtainer).

%% API
-export([
  of_/1,
  value/1,
  map/2
]).

-record(container, {value = undefined}).

of_(X) ->
  #container{value = X}.

value(#container{value = V}) ->
  V.

map(#container{value = V}, F) ->
  of_(F(V)).

