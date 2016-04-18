select(X,[X|Ys],Ys).
select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).


permutation([],[]).
permutation(Xs,[Z|Zs]) :- select(Z,Xs,Ys), permutation(Ys,Zs).

