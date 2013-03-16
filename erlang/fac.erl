-module(fac).
-export([fac/1, tail_fac/1]).
 
fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

tail_fac(N) -> tail_fac(N,1).
 
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc).
