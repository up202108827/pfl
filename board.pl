:- use_module(library(lists)).
:- consult(helper).
:- consult(funcs).



in_bounds(Board, Col-Row) :-
    length(Board, 9),
    length(Board, 17),
    (Row >= 1, Row =< 9),
    (Col >= 1, Col =< 17).


move_piece(Board, Col-Row, empty, NewBoard) :-
    move_piece(Board, Col-Row, nonblock, NewBoard).


move_piece(Board, Col-Row, Piece, NewBoard) :-
    Row2 is Row-1, Col2 is Col-1,
    nth0(Row2, Board, Line),
    replace(Col2, Piece, Line, NewLine),
    replace(Row2, NewLine, Board, NewBoard).


display_game_header(Max) :-
    format('    1   '),
    display_game_header(2, Max).

display_game_header(Max, Max) :-
    format('~d\n  ', [Max]).

display_game_header(N, Max) :-
    (N > 9 ->
        format('~d  ', [N]);
        format('~d   ', [N])
    ),
    N1 is N + 1,
    display_game_header(N1, Max).

get_symbol(Board, Line, Col, Symbol) :-
    position(Board, Col-Line, Piece),
    symbol(Piece, Symbol).

display_pieces(_,_,Col):-
    Col > 17, write('\n  '), !.

display_pieces(Board, Line, Col) :-
    get_symbol(Board, Line, Col, Symbol),
    format(' ~a |', [Symbol]),
    Col1 is Col+1,
    display_pieces(Board, Line, Col1).


display_game_board(_, Line) :-
    Line > 9 ,nl, !.

display_game_board(Board, Line) :-
    format('~d |', [Line]),
    display_pieces(Board, Line, 1),
    display_game_separator(17),
    Line1 is Line+1,
    display_game_board(Board, Line1).


display_game_separator(0) :-
    write('|\n'), !.

display_game_separator(N) :-
    write('|---'),
    N1 is N-1,
    display_game_separator(N1).


position(Board, Col-Row, Piece) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece).


position(Board, Col-Row, Piece) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece),
    Piece \= empty, !.


initialize_board(Board) :-
    board(Board).
