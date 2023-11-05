:- use_module(library(lists)).
:- consult(helper).
:- consult(funcs).



in_bounds(Board, Col-Row) :-
    length(Board, 9),
    length(Board, 17),
    (Row >= 1, Row =< 9),
    (Col >= 1, Col =< 17).

replace(0, NewElement, [_|Tail], [NewElement|Tail]).
replace(Index, NewElement, [Head|Tail], [Head|NewTail]) :-
    Index > 0,
    Index1 is Index - 1,
    replace(Index1, NewElement, Tail, NewTail).


move_piece(Board, Col-Row, empty, NewBoard) :-
    move_piece(Board, Col-Row, nonblock, NewBoard).


move_piece(Board, Col-Row, Piece, NewBoard) :-
    Row2 is Row-1, Col2 is Col-1,
    nth0(Row2, Board, Line),
    replace(Col2, Piece, Line, NewLine),
    replace(Row2, NewLine, Board, NewBoard).

display_game_header(1, Max) :-
    format('    ~d\n  ', [1]).
 

display_game_header(N, Max) :-
    (N =:= Max -> format('~d\n  ', [Max]), !;
    N > 9 ->
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


display_game_board(Board, Line) :-
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


% Initialize the piece lists for both players
extract_pieces([Board,_,_,_,_], WhitePieces, BlackPieces) :-
    extract_pieces(Board, 0, 0, WhitePieces, BlackPieces).

extract_pieces([], _, _, [], []).
extract_pieces([Row | Rest], RowIndex, 0, WhitePieces, BlackPieces) :-
    extract_pieces_in_row(Row, RowIndex, 0, WhitePieces1, BlackPieces1),
    NextRowIndex is RowIndex + 1,
    extract_pieces(Rest, NextRowIndex, 0, WhitePieces2, BlackPieces2),
    append(WhitePieces1, WhitePieces2, WhitePieces),
    append(BlackPieces1, BlackPieces2, BlackPieces).

extract_pieces_in_row([], _, _, [], []).
extract_pieces_in_row([white | Rest], RowIndex, ColIndex, [(RowIndex, ColIndex) | WhitePieces], BlackPieces) :-
    NextColIndex is ColIndex + 1,
    extract_pieces_in_row(Rest, RowIndex, NextColIndex, WhitePieces, BlackPieces).
extract_pieces_in_row([black | Rest], RowIndex, ColIndex, WhitePieces, [(RowIndex, ColIndex) | BlackPieces]) :-
    NextColIndex is ColIndex + 1,
    extract_pieces_in_row(Rest, RowIndex, NextColIndex, WhitePieces, BlackPieces).
extract_pieces_in_row([_ | Rest], RowIndex, ColIndex, WhitePieces, BlackPieces) :-
    NextColIndex is ColIndex + 1,
    extract_pieces_in_row(Rest, RowIndex, NextColIndex, WhitePieces, BlackPieces).


initialize_board(Board) :-
    board(Board), !.
