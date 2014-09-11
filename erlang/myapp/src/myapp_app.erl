-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
				      {'_', [{"/", hello_handler, []}]}
				     ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8888}],
		      [{env, [{dispatch, Dispatch}]}]),
    myapp_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% EUnit
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = application:start(myapp),
    ?assertNot(undefined == whereis(myapp_sup)).

-endif.
