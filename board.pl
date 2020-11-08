:- module(board, [board_size/2, in_range/2, in_board/1, adjacent/2]).

:- dynamic board_size/2.

default_max(x, 5).
default_max(y, 5).

range(x, 1, Xmax) :- board_size(Xmax, _), !.
range(y, 1, Ymax) :- board_size(_, Ymax), !.
range(Axis, 1, Max) :- default_max(Axis, Max), !.

% Walidacja polozenia
in_range(Axe, X) :- range(Axe, Max, Min), X >= Max, X =< Min.
in_board([X, Y]) :- integer(X), integer(Y), in_range(x, X), in_range(y, Y).

% Sąsiadujące pola
adjacent(F1, F2) :- next_field(F1, F2, _), in_board(F2).

next_field([X, Y], [X, Ynext], north) :- Ynext is Y + 1.
next_field([X, Y], [Xnext, Y], west) :- Xnext is X - 1.
next_field([X, Y], [Xnext, Y], east) :- Xnext is X + 1.
next_field([X, Y], [X, Ynext], south) :- Ynext is Y - 1.
next_field([X, Y], [Xnext, Ynext], northwest) :- Xnext is X - 1, Ynext is Y + 1.
next_field([X, Y], [Xnext, Ynext], northeast) :- Xnext is X + 1, Ynext is Y + 1.
next_field([X, Y], [Xnext, Ynext], southwest) :- Xnext is X - 1, Ynext is Y - 1.
next_field([X, Y], [Xnext, Ynext], southeast) :- Xnext is X + 1, Ynext is Y - 1.