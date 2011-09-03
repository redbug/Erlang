-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).
-vsn(1234).
-author("Redbug Chao").

add(A,B)->A+B.

%%Show gretting.
hello()->io:format("hello world!~n").

greet_and_add_two(X)->
	hello(),
	add(X,2).
