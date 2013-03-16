-module(len).
-export([len/1, tail_len/1]).

len([]) -> 0;
len([_ | T]) -> 1 + len(T).

tail_len(L) -> tail_len(L, 0).
 
tail_len([], Acc) -> Acc;
tail_len([_ | T], Acc) -> tail_len(T, Acc + 1).
