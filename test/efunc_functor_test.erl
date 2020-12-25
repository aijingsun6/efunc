-module(efunc_functor_test).

-include_lib("eunit/include/eunit.hrl").

map_test() ->
  L = [
    #{age => 10, name => <<"a10">>, money => 100},
    #{age => 10, name => <<"b10">>, money => 200},
    #{age => 11, name => <<"a11">>, money => 300},
    #{age => 11, name => <<"b11">>, money => 400}
  ],
  Functor = efunc_functor:new(L),
  Functor2 = efunc_functor:pipe(Functor, [
    efunc:filter(fun(#{age := Age}) -> Age == 10 end),
    efunc:foldl(fun(#{money := M}, Acc) -> M + Acc end, 0)
  ]),
  300 = efunc_functor:value(Functor2),
  Functor3 = efunc_functor:pipe(Functor, [
    efunc:filter(fun(#{age := Age}) -> Age == 11 end),
    efunc:foldl(fun(#{money := M}, Acc) -> M + Acc end, 0)
  ]),
  700 = efunc_functor:value(Functor3).