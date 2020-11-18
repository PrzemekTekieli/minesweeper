:- use_module(utils).
:- use_module(board).
:- use_module(algorithm).

:- dynamic hidden/1, field/2, mine/1.

% Inicjalizacja nowej gry
init_state(X,Y) :-
	asserta(board_size(X, Y)),
	locate_hidden.
  
% Usunięcie poprzednich danych
cleanup :-
	retractall(board_size(_, _)),
	retractall(hidden([_, _])),
	retractall(field([_, _], _)),
	retractall(mine([_, _])).
	
% Zaznacz wszystkie pola jako nieodkryte
locate_hidden :-
	locate_hidden(1,1).
locate_hidden(X,Y):- 
	in_range(y,Y), assertz(hidden([X,Y])), Ynext is Y + 1, locate_hidden(X,Ynext).
locate_hidden(X,_) :-
	Xnext is X + 1, in_range(x,Xnext), Ynext is 1, locate_hidden(Xnext,Ynext).
locate_hidden(_,_).

% Załóż że na danym polu jest dana wartość (wiedza ta powinna pochodzić z zewnętrznego źródła)
assert_fields([]).
assert_fields([F,V|T]) :- assert_field(F,V), assert_fields(T), !.

assert_field(F,x) :- retract(hidden(F)), assertz(mine(F)).
assert_field(F,V) :- retract(hidden(F)), assertz(field(F,V)).

% Stwórz nową planszę
start(X,Y) :- cleanup, init_state(X,Y), !.

% W każdym kroku sprawdzane jest czy można uzyskać nowe informacje na podstawie obecnej bazy wiedzy (znaleźć pole na pewno z miną lub na pewno bez niej), jeśli nie, pobieramy pierwsze w kolejności nieodkryte pole i je odkrywamy
next_step(Fields,Mines) :- satisfy_fields(Fields,Mines), !.
next_step(Field,[]) :- hidden(Field), !.%, search_field(Field), !.

% Wykonaj kolejny krok i narysuj obecny stan
step_and_draw(Fields,Mines) :- next_step(Fields,Mines), draw, !.