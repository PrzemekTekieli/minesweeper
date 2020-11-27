:- module(utils, [write_field/1, write_field/2, draw/0, get_sublist/3, in_all_lists/2, sum_results/3]).

% Wypisz pole
write_field([X,Y]) :- write("["), write(X), write(","), write(Y), write("]").

% Wypisz pole z wartością
write_field([X,Y],V) :- write_field([X,Y]), write(":"), write(V), write("\n").

% Wypisz obecną planszę
draw :- draw(1,1), !.
draw(X,Y) :-
	in_range(y,Y), field([X,Y],V), write(V), Ynext is Y + 1, draw(X,Ynext).
draw(X,Y) :-
	in_range(y,Y), mine([X,Y]), write(x), Ynext is Y + 1, draw(X,Ynext).
draw(X,Y) :-
	in_range(y,Y), write(n), Ynext is Y + 1, draw(X,Ynext).
draw(X,_) :-
	write('\n'), Xnext is X + 1, in_range(x,Xnext), Ynext is 1, draw(Xnext,Ynext).
draw(_,_).

% Pobiera możliwe podlisty (o długości co najmniej 2), gdzie kolejność nie ma znaczenia ([a,b] <=> [b,a])
get_sublist([],_,[]).
get_sublist([X|T], 2, [X,Y]) :- select(Y,T,_).
get_sublist([X|T], N, [X|L]) :- N1 is N - 1, get_sublist(T, N1, L), L \= [].
get_sublist([_|T], N, L) :- get_sublist(T,N,L), L \= [].

% Pobiera element, który znajduje się w każdej liście
in_all_lists([],_).
in_all_lists([H|T], X) :- member(X,H), in_all_lists(T,X).

% Wypłaszcza listę list i sumuje jej zawartość (bez powtórzeń) [[X1,Y1],[X2,Y2],...] -> [X1+X2+...,Y1+Y2+...]
sum_results([],[],[]).
sum_results([[F,M]|T],NewFields,NewMines) :- sum_results(T,Fields,Mines), append(F,Fields,NF), append(M,Mines,NM), sort(NF,NewFields), sort(NM,NewMines).