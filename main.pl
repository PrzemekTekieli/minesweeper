:- use_module(utils).
:- use_module(external).
:- use_module(board).

:- dynamic hidden/1, field/2, mine/1.

% Inicjalizacja nowej gry
init_state :-
	asserta(board_size(5, 5)),
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

% Przeszukaj listę pól
search_fields([]).
search_fields([F|T]) :- search_field(F), search_fields(T).

% Przeszukaj pojedyńcze pole
search_field(F) :-
	retract(hidden(F)),
	check_external(F).
	
% Uzyskaj wartość z zewnętrznej wiedzy
check_external(F) :-
	external_knowledge(F,x), assertz(mine(F)), write_field(F,m).
check_external(F) :-
	external_knowledge(F,N), assertz(field(F,N)), write_field(F,N).
	
% Zlicz miny w sąsiedztwie
count_mines(F1, N) :-
	aggregate_all(count, (adjacent(F1,F2), mine(F2)), N).

% Zlicz nieodkryte pola w sąsiedztwie
count_hidden(F1, N) :-
	aggregate_all(count, (adjacent(F1,F2), hidden(F2)), N).

% Pobierz listę nieodkrytych pól w sąsiedztwie
get_hidden(F1,L) :-
	findall(F2, (adjacent(F1,F2), hidden(F2)), L).

% Oznacz listę pól jako miny
mark_mines([]).
mark_mines([F|T]) :- retract(hidden(F)), assertz(mine(F)), write_field(F,m), mark_mines(T).

% Sprawdza zależność pola. Szukamy tylko pól które mają co najmniej jednego nieodkrytego sąsiada
satisfy_fields :- 
	field(F,V), count_hidden(F, N), N \= 0, satisfy(F,V).
% 1. znaleziono już wszystkie miny w okolicy danego pola -> odkryj wszystkich nieodkrytych sąsiadów
satisfy(F,V) :-
	count_mines(F,V), get_hidden(F, L), search_fields(L). 
% 2. liczba brakujących min jest równa liczbie nieodkrytych sąsiadów -> załóż że nieodkryci sąsiedzi to miny
satisfy(F,V) :-
	count_mines(F,N1), count_hidden(F,N2), V is N1 + N2, get_hidden(F, L), mark_mines(L).
%3. liczba brakujących min jest mniejsza niż liczba nieodkrytych sąsiadów -> można próbować sprawdzić czy każda kombinacja jest możliwa (czy wstawienie miny w danym miejscu nie przekroczy limitu min, albo...).



%sprawdzenie czy wstawienie min w dane miejsca z listy, nie sprawią że limit min obok pola zostanie przekroczony
try_assert_mines([]).
try_assert_mines([F|T]) :-
	assertz(mine(F)), (\+ check_global_condition, try_assert_mines(T), retract(mine(F))); retract(mine(F)), false.

check_global_condition :-
	field(F,V), count_mines(F,N1), N1 > V.

%tymczasowa funkcja do testów, pozwala na ustawienie początkowo znanych pól
load_some_knowledge :-
	search_field([2,2]).

start :- cleanup, init_state, load_some_knowledge.

% W każdym kroku sprawdzane jest czy można uzyskać nowe informacje na podstawie obecnej bazy wiedzy (znaleźć pole na pewno z miną lub na pewno bez niej), jeśli nie, pobieramy pierwsze w kolejności nieodkryte pole i je odkrywamy
next_step :- satisfy_fields.
next_step :- hidden(F), search_field(F), !.

step_and_draw :- next_step, draw, !.