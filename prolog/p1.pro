add(0,X,X).
add(g(X),Y,g(Z)):-add(X,Y,Z).
sub(g(X),Y,g(Z)):-add(Y,Z,X).
mul(0,X,0).
mul(g(X),Y,Z1):-mul(X,Y,Z),add(Y,Z,Z1).

my_last(H,[H]).
my_last(L,[H|T]):-my_last(L,T).


lastbutone(H1,[H1,_]).
lastbutone(L,[H1|T]):-lastbutone(L,T).

kthelem([H|T],1,H).
kthelem([H|T],X,ANS):-X1 is X-1,kthelem(T,X1,ANS).

listlength([],0).
listlength([H|T],ANS):-listlength(T,ANS1),ANS is ANS1+1.


list([],0).
list([X|Y],g(L)) :-list(Y,L). 
list_revrse([H],[H]).
list_revrse([X|Y],L):-list_revrse(Y,L1),append(L1,[X],L).

palin(S):-list_revrse(S,L),palindrom(S,L).
palindrom([],[]).
palindrom([H1|T1],[H2|T2]):-H1==H2,palindrom(T1,T2).


flatten([H|T],ANS):-(is_list(H)->flatten(H,ANS);flatten(T,ANS1),append([H],ANS1,ANS)).
flatten([H],[H]).

compress([H1,H2|T],ANS):-H1==H2->compress([H2|T],ANS);compress([H2|T],ANS1),append([H1],ANS1,ANS).
compress([H1],[H1]).

dupli([H|T],ANS):-dupli(T,ANS1),append([H,H],ANS1,ANS).
dupli([],[]).

dupli1([H|T],X,ANS):-dupli1(H,ANS1),do(H,X,ANS).
do(H,X,ANS):-append([H],ANS,ANS).
dupli([],[]).


ins(X,L,[X:L]).
ins(X,[H|T],ANS):-ins(X,T,ans).

binvalue([X|Y],ANS):-listlength([X|Y],Z),myvalue([X|Y],Z,ANS).

myvalue([X|Y],Z,ANS):-Z1 is Z-1,myvalue(Y,Z1,ANS1),ANS is ANS1+X*(2**Z1).
myvalue([],0,0).

p(X):-q(X,Y),Z is X+Y, r(Z).
p(X):-r(X),X>3.
q(X,X):-r(X).
q(1,2).
r(2).
r(3).
r(4).


mystery([H|T]):-mys(H,T).
mys(X,[X]).
mys(X,[_|T]):-mys(X,T).




twice(X,[H|T]):-twice1(X,[H|T],2).
twice1(X,[X|_],1).
twice1(X,[X|T],Count):-Count1 is Count-1,twice1(X,T,Count1).
twice1(X,[_|T],Count):-Count>1->twice1(X,T,Count).



enum(0).
enum(N):-enum(M),N is M+1.
enum(N,M):-enum(M),M>N.
append([],L,L).
append([X1|Y1],Y2,[X1|Z1]):-append(Y1,Y2,Z1).
sum_list([],0).
sum_list([X|Y],ANS):-sum_list(Y,Z),ANS is X+Z.


fact(0,1).
fact(N,L):-N>0,N1 is N-1,fact(N1,L1),L is N*L1.



pi('a','b').
pi('a','c').
pi('b','c').
pi('c',X):-pi('b',Z),pi('a',X).
qi(X,Y):-pi(X,Y),pi(Y,X).
qi(X,Y):-pi(X,X).



mortal(X):-human(X).
human(socrates).

man(X):-mortal(X),!,write(X),fail.
man(X).


parent(john,paul).   
parent(paul,tom).    
parent(tom,mary).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z),ancestor(Z,Y).

facti(0,1).
facti(N,ANS):-N1 is N-1,facti(N1,ANS1),ANS is ANS1*N,!.


bindec(X,ANS):-reverse(X,X1),cal(X1,ANS).
cal([X|H],ANS):-cal(H,ANS1),ANS is ANS1*2+X.
cal([],0).



count(_,[],0).
count(A,[A|L],N):- !,count(A,L,N1),N is N1+1.
count(A,[_|L],N):- count(A,L,N). 
