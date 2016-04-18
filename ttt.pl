:- use_module(library(apply)).

repeat(_,0,R) :- R = [],!.
repeat(X,N,R) :- N1 is N-1, repeat(X,N1,R1), R = [X|R1]. 
insert(X, [], R) :- R = [X], !.
insert(X, [H|T], R) :- R = [X,H|T].

perm_helper([],[[]]).
perm_helper([H|T],R) :- perm_helper(T,R1), length(R1,L), repeat(H,L,R2), maplist(insert,R2,R1,R).





link(a,b).
link(a,c).
link(b,d).
link(c,d).
link(c,f).
link(d,e).
link(d,f).
link(f,a).


path(A,B) :- link(A,B).
path(A,B) :- link(A,C), path(C,B). 

path(StartNode, EndNode, [StartNode,EndNode]) :- link(StartNode,EndNode).
path( StartNode, EndNode, [ StartNode | Rest]) :- link( StartNode, NextNode),
                                                  path( NextNode, EndNode, Rest).

conc( [], L, L).
conc( [X|L1], L2, [X | L3]) :- conc( L1, L2, L3).

% Figure 4.9   Program 2 for the eight queens problem.


% solution( Queens) if 
%   Queens is a list of Y-coordinates of eight non-attacking queens

solution( Queens)  :-
   permutation( [1,2,3,4,5,6,7,8], Queens),
   safe( Queens). 



permutation([],[]).

permutation([H|T],R) :- permutation(T,R1),
                        insert(H,R1,R).





%permutation( [Head | Tail], PermList)  :-
%   permutation( Tail, PermTail),
%   del( Head, PermList, PermTail).   % Insert Head in permuted Tail 

% del( Item, List, NewList): deleting Item from List gives NewList

del( Item, [Item | List], List). 

del( Item, [First | List], [First | List1] )  :-
   del( Item, List, List1). 

% safe( Queens) if 
%   Queens is a list of Y-coordinates of non-attacking queens

safe( [] ). 

safe( [Queen | Others] )  :-
   safe( Others),
   noattack( Queen, Others, 1). 

noattack( _, [], _). 

noattack( Y, [Y1 | Ylist], Xdist)  :-
   Y1-Y =\= Xdist,
   Y-Y1 =\= Xdist,
   Dist1 is Xdist + 1,
   noattack( Y, Ylist, Dist1).




