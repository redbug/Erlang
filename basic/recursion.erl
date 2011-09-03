-module(recursion).
-export([fac/1, fac2/1, len/1, len2/1, tail_fac/1, duplicate/2, tail_duplicate/2, 
	reverse/1, tail_reverse/1, sub_list/2, tail_sub_list/2, zip/2, tail_zip/2,
	lc_quicksort/1]).

fac(N) when N ==0 -> 1;
fac(N) when N >0 -> N * fac(N-1).

fac2(0) -> 1;
fac2(N) when N > 0 -> N*fac2(N-1).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

%% tail recursion version of len
len2(L) -> len2(L, 0).

len2([], Acc) -> Acc;
len2([_|T], Acc) -> len2(T, Acc+1).


% tail recursion version of fac()
tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).


duplicate(0,_) -> [];
duplicate(N,Term) when N > 0 ->
	[Term|duplicate(N-1,Term)].

% tail recursion version
tail_duplicate(N,Term) -> tail_duplicate(N,Term,[]).

tail_duplicate(0,_,Acc) -> Acc;
tail_duplicate(N,Term,Acc) when N > 0 -> tail_duplicate(N-1,Term,[Term|Acc]).


reverse([])->[];
reverse([H|T])->reverse(T)++[H].

% tail recursion version
tail_reverse(L)->tail_reverse(L,[]).

tail_reverse([],RL)->RL;
tail_reverse([H|T],RL)->tail_reverse(T, [H|RL]).


sub_list([],_)->[];
sub_list(_,0)->[];
sub_list([H|T],N) -> [H|sub_list(T,N-1)].

% tail recursion version
% note: we must reverse the final result ourselves here.
tail_sub_list(L,N) -> lists:reverse(tail_sub_list(L,N,[])).

tail_sub_list([],_,List) -> List;
tail_sub_list(_,0,List) -> List;
tail_sub_list([H|T],N,List) ->
	tail_sub_list(T,N-1,[H|List]).


zip([],_)->[];
zip(_,[])->[];
zip([H1|T1], [H2|T2])->[{H1,H2}|zip(T1,T2)].

tail_zip(X,Y)->lists:reverse(tail_zip(X,Y,[])).

tail_zip([],_,R)->R;
tail_zip(_,[],R)->R;
tail_zip([H1|T1], [H2|T2], R) -> tail_zip(T1, T2, [{H1,H2}|R]).


lc_quicksort([])->[];
lc_quicksort([Pivot|Rest])->
	lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
	++ [Pivot] ++
	lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
