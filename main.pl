:- use_module(utils).
:- use_module(board).
:- use_module(algorithm).

:- dynamic hidden/1, holder/1, completed/2, field/2, mine/1.

% Inicjalizacja nowej gry
init_state(X,Y,M) :-
	asserta(board_size(X, Y)),
	asserta(board_mines(M)),
	locate_hidden.
  
% Usunięcie poprzednich danych
cleanup :-
	retractall(board_size(_, _)),
	retractall(board_mines(_)),
	retractall(hidden([_, _])),
	retractall(holder([_, _])),
	retractall(completed([_, _], _)),
	retractall(field([_, _], _)),
	retractall(mine([_, _])).

% Hidden - nieodkryte pole
% Holder - pole w którym wiadomo że nie ma miny (nadal czeka na odkrycie)
% Field - odkryte pole, które posiada co najmniej jedno nieodkryte pole w sąsiedztwie
% Completed - odkryte pole, które ma wszystkich odkrytych sąsiadów
% Mine - pole będące miną

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

assert_field(F,x) :- (hidden(F), retract(hidden(F));holder(F),retract(holder(F))), assertz(mine(F)).
assert_field(F,V) :- (hidden(F), retract(hidden(F));holder(F),retract(holder(F))), assertz(field(F,V)).

% Oznacz listę pól jako pola bez miny (ale nie odkryte)
mark_holders([]).
mark_holders([F|T]) :- retract(hidden(F)), assertz(holder(F)), mark_holders(T).

% Oznacz listę pól jako miny
mark_mines([]).
mark_mines([F|T]) :- retract(hidden(F)), assertz(mine(F)), write_field(F,m), mark_mines(T).

% Oznacz pole jako w pełni otoczone
mark_completed(F,V) :- retract(field(F,V)), assertz(completed(F,V)).

% Stwórz nową planszę
start(X,Y) :- cleanup, init_state(X,Y,100), !.

% Stwórz nową planszę (z podaniem łącznej liczby min)
start(X,Y,M) :- cleanup, init_state(X,Y,M), !.

% W każdym kroku sprawdzane jest czy można uzyskać nowe informacje na podstawie obecnej bazy wiedzy (znaleźć pole na pewno z miną lub na pewno bez niej), jeśli nie, pobieramy pierwsze w kolejności nieodkryte pole i je odkrywamy (Fields - pola do odkrycia, Mines - miejsca z minami, Knowledge - czy to wyliczony wybór, czy losowy)
next_step(Fields,Mines,1) :- check_completed, satisfy_fields(Fields,Mines), mark_holders(Fields), mark_mines(Mines), !.
next_step(Field,[],0) :- search_anyfield(Field), !.

% Pomocnicza funkcja, wykorzystywana w all_next_Steps, jeżeli bezpośrednio próbuje się zrobić finall na satisfy_fields, to pojawia się problem ze zwracanymi wartościami.
tmp_step(Fields,Mines) :- satisfy_fields(Fields,Mines).

% Pobiera wszystkie obecnie możliwe kroki, czyli pola do odkrycia i miny do zaznaczenia (miny zaznacza samodzielnie). jeśli nic nie znajdzie, to zwraca pierwsze nieodkryte pole, argumenty jak wyżej
all_next_steps(Fields,Mines,1) :- check_completed, findall([F,M],tmp_step(F,M),Result), sum_results(Result,Fields,Mines), (Fields \= []; Mines \= []), mark_holders(Fields), mark_mines(Mines), !.
all_next_steps(Field,[],0) :- search_anyfield(Field), !.

% Wykonaj kolejny krok i narysuj obecny stan
step_and_draw(Fields,Mines) :- next_step(Fields,Mines), draw, !.