:- module(utils, [write_field/1, write_field/2, draw/0]).

% Wypisz pole
write_field([X,Y]) :- write("["), write(X), write(","), write(Y), write("]").

% Wypisz pole z wartością
write_field([X,Y],V) :- write_field([X,Y]), write(":"), write(V), write("\n").

% Wypisz obecną planszę
draw :- draw(1,1).
draw(X,Y) :-
	in_range(y,Y), field([X,Y],V), write(V), Ynext is Y + 1, draw(X,Ynext).
draw(X,Y) :-
	in_range(y,Y), mine([X,Y]), write(x), Ynext is Y + 1, draw(X,Ynext).
draw(X,Y) :-
	in_range(y,Y), write(n), Ynext is Y + 1, draw(X,Ynext).
draw(X,_) :-
	write('\n'), Xnext is X + 1, in_range(x,Xnext), Ynext is 1, draw(Xnext,Ynext).
draw(_,_).