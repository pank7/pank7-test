-module(robust_bank).

-export([deposit/3, withdraw/2, balance/1]).

-include("bank.hrl").

deposit(Tag, Who, X) ->
    Fun = 
	fun() ->
		case mnesia:read({cached, Tag}) of
		    [] ->
			%% no cached result
			case mnesia:read({account, Who}) of
			    [] ->
				%% no account so we make one
				Entry = #account{name=Who,balance=X},
				mnesia:write(Entry),
				update_cache(Who, Tag, X),
				X;
			    [E] ->
				Old = E#account.balance,
				New = Old + X,
				E1 = E#account{balance=New},
				mnesia:write(E1),
				update_cache(Who, Tag
				New
			end;
		    [C] ->
			C#cached.value
		end,
		mnesia:transaction(Fun).


balance(Who) ->
    Fun = 
	fun() ->
	   case mnesia:read({account, Who}) of
	       [] ->
		   %% no account
		   {error, no_such_account};
	       [E] ->
		   B = E#account.balance,
		   {ok, B}
	   end
	end,
    mnesia:transaction(Fun).

withdraw(Who, X) ->
    Fun = 
	fun() ->
	   case mnesia:read({account, Who}) of
	       [] ->
		   %% no account
		   {error, no_such_user};
	       [E] ->
		   Old = E#account.balance,
		   if 
		       Old >= X ->
			   New = Old - X,
			   E1 = E#account{balance=New},
			   mnesia:write(E1),
			   ok;
		       Old < X ->
			   {error, not_enough_money}
		   end
	   end
	end,
    mnesia:transaction(Fun).
