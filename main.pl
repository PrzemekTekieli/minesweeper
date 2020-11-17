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
	
% Pobierz listę odkrytych pól (nie będących minami) w sąsiedztwie
get_fields(F1,L) :-
	findall(F2, (adjacent(F1,F2), field(F2,_)), L).

% Oznacz listę pól jako miny
mark_mines([]).
mark_mines([F|T]) :- retract(hidden(F)), assertz(mine(F)), write_field(F,m), mark_mines(T).

% Sprawdź zależność pola. Szukamy tylko pól które mają co najmniej jednego nieodkrytego sąsiada
satisfy_fields :- 
	field(F,V), count_hidden(F,H), H \= 0, count_mines(F,M), get_hidden(F,HL), satisfy(F,V,M,H,HL).
% 1. znaleziono już wszystkie miny w okolicy danego pola -> odkryj wszystkich nieodkrytych sąsiadów
satisfy(_,V,M,_,HL) :-
	V = M, search_fields(HL). 
% 2. liczba brakujących min jest równa liczbie nieodkrytych sąsiadów -> załóż że nieodkryci sąsiedzi to miny
satisfy(_,V,M,H,HL) :-
	V is M + H, mark_mines(HL).
% 3. liczba brakujących min jest mniejsza niż liczba nieodkrytych sąsiadów -> próba zawężenia możliwych rozmieszczeń min
% a) brakuje tylko jednej miny -> może jest tylko jedno możliwe miejsce na tą minę?
% ?czy taka sytuacja jest w ogóle możliwa?
satisfy(F,V,M,_,HL) :-
	1 is V - M, findall(L, (select(L,HL,_), try_assert_mines(L)), ML), possible_mines(F,ML).
% b) brakuje kilku min -> może da się zawęzić liczbę możliwych miejsc do ilości min?
satisfy(F,V,M,_,HL) :-
	MR is V - M, MR > 1, get_fields(F,FL), findall(L, (get_sublist(HL,MR,L), try_assert_mines2(L,FL)), ML), possible_mines(F,ML).

% Sprawdzenie możliwego rozmieszczenia min dla podanego pola
% 0. nie można ustawić min
possible_mines(_,[]) :- false.
% 1. miny można rozmieścić w tylko jeden sposób -> rozmieść miny w podany sposób
possible_mines(_,[M|[]]) :- mark_mines(M).
% 2. jedno z pól zawiera minę w każdym możliwym rozmieszczeniu -> zaznacz że jest tam mina
possible_mines(_,ML) :- in_all_lists(ML,X), X \= [], mark_mines([X]).

% Sprawdzenie czy wstawienie min w dane miejsca z listy, nie sprawi że limit min obok któregokolwiek pola zostanie przekroczony. Dodatkowo sprawdza czy po takiej operacji, każdy z elementów podanej listy pól ma szansę mieć jakiekolwiek rozwiązanie.
try_assert_mines([],[]).
try_assert_mines([],[F|T]) :- field(F,V), count_mines(F,M), 0 is V - M, try_assert_mines([],T).
try_assert_mines([],[F|T]) :- field(F,V), count_mines(F,M), 0 < V - M, get_hidden(F,HL), try_assert_mines(HL,T).
try_assert_mines([F|T],L) :-
	assertz(mine(F)), (\+ check_global_condition, try_assert_mines(T,L), retract(mine(F))); retract(mine(F)), false.

% Sprawdzenie czy istnieje jakieś pole posiadające obok siebie więcej min niż powinno
check_global_condition :-
	field(F,V), count_mines(F,M), M > V.

% tymczasowa funkcja do testów - pozwala założyć że na danym polu jest dana wartość
assert_fields([]).
assert_fields([F,V|T]) :- assert_field(F,V), assert_fields(T).

assert_field(F,x) :- retract(hidden(F)), assertz(mine(F)).
assert_field(F,V) :- retract(hidden(F)), assertz(field(F,V)).
	
%tymczasowa funkcja do testów - wczytuje początkowe ustawienia
load_some_knowledge :-
	%search_fields([[1,1],[1,2],[1,3]]).
	%search_fields([[2,2]]).
	%search_fields([[4,5],[5,3],[5,4]]),
	search_fields([[4,4],[4,5],[5,4]]),
	%assert_fields([[1,1],1,[1,2],2,[2,3],3]),
	draw.

start :- cleanup, init_state, load_some_knowledge.

% W każdym kroku sprawdzane jest czy można uzyskać nowe informacje na podstawie obecnej bazy wiedzy (znaleźć pole na pewno z miną lub na pewno bez niej), jeśli nie, pobieramy pierwsze w kolejności nieodkryte pole i je odkrywamy
next_step :- satisfy_fields.
next_step :- hidden(F), search_field(F), !.

step_and_draw :- next_step, draw, !.