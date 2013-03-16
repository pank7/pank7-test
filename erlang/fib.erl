-module(fib).
-export([fib/1, fib2/1, fib3/1]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when N > 1 ->
    fib(N - 1) + fib(N - 2);
fib(_) ->
    error.

fib2(N) when N >= 0 ->
    fib2(N, 0, 1);
fib2(_) ->
    error.

fib2(0, Result, _) ->
    Result;
fib2(N, Result, _Rest) when N > 0 ->
    fib2(N - 1, _Rest, Result + _Rest).

fib3(N) when N >=0 ->
    {Fib, _} = fibo3(N, {1, 1}, {0, 1}),
    Fib;
fib3(_) ->
    error.

fibo3(0, _, Pair) ->
    Pair;
fibo3(N, {Fib1, Fib2}, Pair) when N rem 2 == 0 ->
    SquareFib1 = Fib1 * Fib1,
    fibo3(N div 2, {2 * Fib1 * Fib2 - SquareFib1, SquareFib1 + Fib2 * Fib2}, Pair);
fibo3(N, {FibA1, FibA2} = Pair, {FibB1, FibB2}) ->
    fibo3(N - 1, Pair, {FibA1 * FibB2 + FibB1 * (FibA2 - FibA1), FibA1 * FibB1 + FibA2 * FibB2}).
