-module(tut).
-export([double/1, useless/0, useless/1]).

double(X) ->
    2 * X.

useless() ->
    {io:format(<<"Hello, world!~n">>)}.

useless(pank7) ->
    {io:format(<<"Hello, my master!~n">>)};
useless("pank7") ->
    {io:format(<<"Hello, my master!~n">>)};
useless(<<"pank7">>) ->
    {io:format(<<"Hello, my master!~n">>)};
useless(Name) ->
    {io:format(<<"Hello, ~s!~n">>, [Name]), Name}.
