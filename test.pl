:-table fib/2.

fib(0, 1):-!.
fib(1, 1):-!.
fib(N,F):-N>1,N1 is N-1, N2 is N-2,fib(N1,F1),fib(N2,F2),F is F1+F2.



sumto(1,1):-!.
sumto(N,S):-N1 is N-1,sumto(N1,S1), S is S1 + N.

sumlist([], N, S) :- S = N.
sumlist([H|T], N, S) :- N1 is N + H,
                        sumlist(T, N1, S).

myappend([],L,L).
myappend([H|T],L2,[H|L3]) :- myappend(T,L2,L3).

word(article,a).
word(article,every).
word(noun,criminal).
word(noun,'big kahuna burger').
word(verb,eats).
word(verb,likes).

sentence(Word1,Word2,Word3,Word4,Word5) :-
		word(article,Word1),
		word(noun,Word2),
		word(verb,Word3),
		word(article,Word4),
		word(noun,Word5).


tran(eins,one).
tran(zwei,two).
tran(drei,three).
tran(vier,four).
tran(fuenf,five).
tran(sechs,six).
tran(sieben,seven).
tran(acht,eight).
tran(neun,nine).


listtran([], []).
listtran([First|T], [Mean|EngT]):-(tran(First, Mean);tran(Mean, First)),listtran(T, EngT).

/*
word(abalone,a,b,a,l,o,n,e).
word(abandon,a,b,a,n,d,o,n).
word(enhance,e,n,h,a,n,c,e).
word(anagram,a,n,a,g,r,a,m).
word(connect,c,o,n,n,e,c,t).
word(elegant,e,l,e,g,a,n,t).

crossword(V1,V2,V3,H1,H2,H3) :- word(V1,V11,V12,V13,V14,V15,V16,V17),
                                word(V2,V21,V22,V23,V24,V25,V26,V27),
                                word(V3,V31,V32,V33,V34,V35,V36,V37),
                                word(H1,H11,V12,H13,V22,H15,V32,H17),
                                word(H2,H21,V14,H23,V24,H25,V34,H27),
                                word(H3,H31,H16,H33,H26,H35,V36,H37).
*/


mult(N,L,S) :- N = 1, S = L.
mult(N,L,S) :- N1 is N-1,
               Y is N mod 3,
               Z is N mod 5,
               ((Y = 0, Z = 0) -> mult(N1, [N|L], S); mult(N1, L, S) ).


numeral(0).
numeral(succ(X)) :- numeral(X).


directTrain(forbach,saarbruecken).
directTrain(freyming,forbach).
directTrain(fahlquemont,stAvold).
directTrain(stAvold,forbach).
directTrain(saarbruecken,dudweiler).
directTrain(metz,fahlquemont).
directTrain(nancy,metz).


travelBetween(X,A,Y) :- directTrain(X,Y), not(member(X,A)), append([X],A, R).
travelBetween(X,A,Y) :- directTrain(Y,X), not(member(X,A)), append([X],A, R).
travelBetween(X,A,Y) :- directTrain(X,Z), not(member(X,A)), append([X],A, R), travelBetween(Z,R,Y).
travelBetween(X,A,Y) :- directTrain(Z,X), not(member(X,A)), append([X],A, R), travelBetween(Z,R,Y).


reverse([])     --> [].
reverse([L|Ls]) --> reverse(Ls), [L].

naiverev([],[]).
naiverev([H|T],R):-  naiverev(T,RevT),  append(RevT,[H],R).

accRev([H|T],A,R):-  accRev(T,[H|A],R).
accRev([],A,A).

rev(L,R) :- accRev(L,[],R).

every_second([],A,L,R) :- R = L.
every_second([H|T], A, L,R) :- A1 = not(A),
                                     ( A -> append(L,[H],L1); L1 = L),
                                     every_second(T, A1,L1,R).

every_second(L,R) :- every_second(L,false, [], R).

mtwice([],[]).
mtwice([H|T],R):-  mtwice(T,Y),  append([H,H],Y, R).


alpha(LD):-
	LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],
	domain(LD,1,26),

	alldifferent(LD),

	B+A+L+L+E+T       #= 45,
	C+E+L+L+O         #= 43,
	C+O+N+C+E+R+T     #= 74,
	F+L+U+T+E         #= 30,
	F+U+G+U+E         #= 50,
	G+L+E+E           #= 66,
	J+A+Z+Z           #= 58,
	L+Y+R+E           #= 47,
	O+B+O+E           #= 53,
	O+P+E+R+A         #= 65,
	P+O+L+K+A         #= 59,
	Q+U+A+R+T+E+T     #= 50,
	S+A+X+O+P+H+O+N+E #= 134,
	S+C+A+L+E         #= 51,
	S+O+L+O           #= 37,
	S+O+N+G           #= 61,
	S+O+P+R+A+N+O     #= 82,
	T+H+E+M+E         #= 72,
	V+I+O+L+I+N       #= 100,
	W+A+L+T+Z         #= 34,

	labeling(LD).

happy(me).
happy(you).

write_everybody_happy :- happy(X),
                         write(X),nl,
                         fail.
write_everybody_happy :- true.
