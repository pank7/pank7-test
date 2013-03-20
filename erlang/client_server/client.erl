-module(client).

%% Socket client routines
%% Author: Joe Armstrong <joe@sics.se>
%% Date:   2003-10-02

-export([tests/1, test1/1, test2/1, test3/1]).

tests(Port) ->
    spawn(fun() -> test1(Port) end),
    spawn(fun() -> test2(Port) end),
    spawn(fun() -> test3(Port) end).

%% send a single message to the server wait for
%% a reply and close the socket

test1(Port) ->
    case gen_tcp:connect("localhost", Port, [binary,{packet, 2}]) of
	{ok, Socket} ->
	    io:format("Socket=~p~n",[Socket]),
	    gen_tcp:send(Socket, "hello joe"),
	    Reply = wait_reply(Socket),
	    io:format("Reply 1 = ~p~n", [Reply]),
	    gen_tcp:close(Socket);
	_ ->
	    error
    end.

test2(Port) ->
    case gen_tcp:connect("localhost", Port,
			 [binary,{packet, 2}]) of
	{ok, Socket} ->
	    io:format("Socket=~p~n",[Socket]),
	    gen_tcp:send(Socket, "hello joe"),
	    Reply = wait_reply(Socket),
	    io:format("Reply 2 = ~p~n", [Reply]),
	    exit(1);
	_ ->
	    error
    end.

test3(Port) ->
    case gen_tcp:connect("localhost", Port,
			 [binary,{packet, 2}]) of
	{ok, Socket} ->
	    io:format("Socket=~p~n",[Socket]),
	    gen_tcp:send(Socket, [42|"hello joe"]),
	    Reply = wait_reply(Socket),
	    io:format("Reply 3 = ~p~n", [Reply]);
	_ ->
	    error
    end.

wait_reply(X) ->
    receive
	Reply ->
	    {value, Reply}
    after 100000 ->
	    timeout
    end.


