-module(test_functions).
%-export([greet/2, head/1, second/1]).
-compile(export_all).

%% Each function declarations is called a function clause.
%% Function clause must be separated by semicolons(;).
%% and together form a function declaration.

greet(male, Name)->
    io:format("Hello, Mr. ~s!~n", [Name]);
greet(female, Name)->
    io:format("Hello, Mrs. ~s!~n", [Name]);
greet(_, Name)->
    io:format("Hello, ~s!~n", [Name]).

head([H|_])->H.

second([_,X|_])->X.


same(X,X)->true;
same(_,_)->false.

valid_time({Date={Y,M,D}, Time={H,Min,S}})->
    io:format("The Date Tuple (~p) says today is:~p/~p/~p,~n",[Date,Y,M,D]),
    io:format("The Time Tuple (~p) indicates: ~p:~p:~p.~n",[Time,H,Min,S]);
valid_time(_)->
    io:format("Wrong argument!~n").

% Variable starting with underscore (_) are normal variable, not anonymous. 
% They are however ignored by the compiler in the sense that they will not generate any warnings for unused variables.


% Guards are additional clauses that can go in a function's head to make pattern matching more expressive. 
old_enough(_X) when _X >= 16 -> true;
old_enough(_X)->false.


% The comma(,) acts in a similar manner to the operator 'andalso', but not exactly the same.
% see http://learnyousomeerlang.com/syntax-in-functions#highlighter_925645
right_age(_X) when _X >= 16, _X =< 104 -> true;
right_age(_X)->false.


% The semi-colon(;) acts in a similar manner to the operator "orelse', but not exactly the same.
% see http://learnyousomeerlang.com/syntax-in-functions#highlighter_925645
wrong_age(_X) when _X < 16; _X > 104 -> true;
wrong_age(_X)->false.



