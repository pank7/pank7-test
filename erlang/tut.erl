-module(tut).
-export([double/1, useless/1]).

double(X) ->
    2 * X.

useless(A) ->
    {io:format("hello world~n"), A}.

