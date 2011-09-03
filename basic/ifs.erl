-module(ifs).
-compile(export_all).

oh_god(N)->
	if  N =:= 2 -> might_succeed;
	    true -> always_does %% this is Erlang's if's else!.
	end.

help_me(Animal)->
	Talk =	if  Animal == cat -> "meow";
		    Animal == beef -> "mooo";
		    Animal == dog -> "bark";
		    Animal == tree -> "bark";
		    true -> "fdgidfjal"
		end,
	{Animal, "says " ++Talk++"!"}.


