-module(tut).
-export([double/1, useless/0, useless/1, useless2/0, useless2/1]).

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

useless2() ->
    io:format(<<"Everyone is useless.~n">>).

useless2(Name) when Name =:= pank7; Name =:= <<"pank7">> ->
    io:format(<<"pank7 is powerful!~n">>);
useless2(Name) ->
    io:format(<<"~s is useless.~n">>, [Name]).
