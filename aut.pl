start(b1,1).
final(b1,3).
trans(b1,1,2,a).
trans(b1,1,2,the).
trans(b1,2,2,brave).
trans(b1,2,2,fast).
trans(b1,2,3,witch).
trans(b1,2,3,wizard).
trans(b1,2,3,broomstick).
trans(b1,2,3,rat).
trans(b1,1,3,harry).
trans(b1,1,3,ron).
trans(b1,1,3,hermione).
trans(b1,3,1,with).


recognize(A,Node,[]) :- final(A,Node).

recognize(A,Node_1,String) :-
              trans(A,Node_1,Node_2,Label),
              traverse(Label,String,NewString),
              recognize(A,Node_2,NewString).

traverse(Label,[Label|Symbols],Symbols).

test(A,Symbols) :-
              start(A,Node),
              recognize(A,Node,Symbols).
