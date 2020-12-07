:- module(algorithm, [satisfy_fields/2,search_anyfield/1,check_completed/0]).

% Zlicz miny w sąsiedztwie
count_mines(F1, N) :-
	aggregate_all(count, (adjacent(F1,F2), mine(F2)), N).

% Zlicz nieodkryte pola w sąsiedztwie
count_hidden(F1, N) :-
	aggregate_all(count, (adjacent(F1,F2), hidden(F2)), N).

% Pobierz listę nieodkrytych pól w sąsiedztwie (sprawdzając czy nie ma tam tymczasowej miny)
get_hidden(F1,L) :-
	findall(F2, (adjacent(F1,F2), hidden(F2), \+ mine(F2)), L).
	
% Pobierz listę odkrytych pól (nie będących minami) w sąsiedztwie (nie pobiera pól "completed")
get_fields(F1,L) :-
	findall(F2, (adjacent(F1,F2), field(F2,_)), L).

% Odkryj jakiekolwiek wolne pole
search_anyfield(Field) :- hidden(Field).

% Szukamy pól, które mają odryktych wszystkich sąsiadów i zamieniamy je z "field" na "completed"
check_completed :-
	field(F,V), count_hidden(F,H), H = 0, mark_completed(F,V), fail.
check_completed.

% Sprawdź zależność pola. Szukamy tylko pól które mają co najmniej jednego nieodkrytego sąsiada, 
satisfy_fields(Fields,Mines) :- 
	field(F,V), count_hidden(F,H), H \= 0, count_mines(F,M), get_hidden(F,HL), satisfy(F,V,M,H,HL,Fields,Mines).
% 1. znaleziono już wszystkie miny w okolicy danego pola -> odkryj wszystkich nieodkrytych sąsiadów
satisfy(_,V,M,_,HL,Fields,[]) :-
	V = M, Fields = HL.%, search_fields(HL). 
% 2. liczba brakujących min jest równa liczbie nieodkrytych sąsiadów -> załóż że nieodkryci sąsiedzi to miny
satisfy(_,V,M,H,HL,[],Mines) :-
	V is M + H, Mines = HL.
% 3. liczba brakujących min jest mniejsza niż liczba nieodkrytych sąsiadów -> próba zawężenia możliwych rozmieszczeń min
% a) brakuje tylko jednej miny -> może jest tylko jedno możliwe miejsce na tą minę?
% ?czy taka sytuacja jest w ogóle możliwa?
satisfy(F,V,M,_,HL,[],Mines) :-
	1 is V - M, findall(L, get_fields(F,FL), (select(L,HL,_), try_place_mines(L, FL)), ML), possible_mines(F,ML,Mines).
% b) brakuje kilku min -> może da się zawęzić liczbę możliwych miejsc do ilości min?
satisfy(F,V,M,_,HL,[],Mines) :-
	MR is V - M, MR > 1, get_fields(F,FL), findall(L, (get_sublist(HL,MR,L), try_place_mines(L,FL)), ML), possible_mines(F,ML,Mines).

% Sprawdzenie możliwego rozmieszczenia min dla podanego pola
% 0. nie można ustawić min
possible_mines(_,[],[]) :- false.
% 1. miny można rozmieścić w tylko jeden sposób -> rozmieść miny w podany sposób
possible_mines(_,[M|[]],M).
% 2. jedno z pól zawiera minę w każdym możliwym rozmieszczeniu -> zaznacz że jest tam mina
possible_mines(_,ML,[X]) :- in_all_lists(ML,X), X \= [].

% Sprawdzenie czy wstawienie min w dane miejsca z listy, nie sprawi że limit min obok któregokolwiek pola zostanie przekroczony. Dodatkowo sprawdza czy po takiej operacji, każdy z elementów podanej listy pól ma szansę mieć jakiekolwiek rozwiązanie.
try_place_mines([],[]).
try_place_mines([],[F|T]) :- field(F,V), count_mines(F,M), MR is V - M, 0 is MR, try_place_mines([],T).
try_place_mines([],[F|T]) :- field(F,V), count_mines(F,M), MR is V - M, 1 is MR, get_hidden(F,HL), select(L,HL,_), try_place_mines(L,T).
try_place_mines([],[F|T]) :- field(F,V), count_mines(F,M), MR is V - M, MR > 1, get_hidden(F,HL), get_sublist(HL,MR,L), try_place_mines(L,T).
try_place_mines([F|T],L) :-
	assertz(mine(F)), (\+ check_global_condition, try_place_mines(T,L), retract(mine(F))); retract(mine(F)), false.

% Sprawdzenie czy istnieje jakieś pole posiadające obok siebie więcej min niż powinno
check_global_condition :-
	field(F,V), count_mines(F,M), M > V.