-module(test_lambda).
-compile(export_all).

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

% L = [1,2,3,4,5];
% test_lambda:map(fun test_lambda:incr/0, L).
% test_lambda:map(fun(X) -> X +1 end, L),

even(L) -> lists:reverse(even(L,[])).

even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
    even(T, [H|Acc]);
even([_|T], Acc) -> 
    even(T, Acc).

% test_labmda:even([1,2,3,4,5]).

old_men(L) -> lists:reverse(old_men(L, [])).

old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
	old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
	old_men(People, Acc).

% test_lambda:old_men([{male, 65}, {female, 30}, {male, 17}, {male, 90}]).

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
	true -> filter(Pred, T, [H|Acc]);
	false -> filter(Pred, T, Acc)
    end.

% Numbers = lists:seq(1,10).
% test_lambda:filter( fun(X) -> X rem 2 == 0 end, Numbers).

% People = [{male,45},{female,67},{male,66},{female,12},{unkown,174},{male,74}].
% test_lambda:filter( fun({Gender, Age}) -> Gender == male andalso Age > 60 end, People). 


max([H|T]) -> max2(T, H).

max2([], Max) -> Max;
max2([H|T], Max) when H > Max -> max2(T, H);
max2([_|T], Max) -> max2(T, Max).


sum(L) -> sum2(L, 0).

sum2([], Sum) -> Sum;
sum2([H|T], Sum) -> sum2(T, H + Sum).

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H, Start), T).

% [H|T] = [1,7,3,5,9,0,2,3].
% test_lambda:fold( fun(A,B) when A > B -> A; (_,B) -> B end, H, T).

% [H|T] = [1,7,3,5,9,0,2,3].
% test_lambda:fold( fun(A,B) when A < B -> A; (_,B) -> B end, H, T).

% test_lambda:fold( fun(A,B) -> A + B end, 0, lists:seq(1,6) ).
