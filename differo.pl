:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(confs).
:- consult(board).


impossible_move(Board, Col1-Row1, Col2-Row2) :-
    delta(Col1, Col2, DeltaCol),
    delta(Row1, Row2, DeltaRow),
    move_direction(DeltaCol, Hordir),
    move_direction(DeltaRow, Verdir),
    \+ impossible_move_aux(Board, Col1, Row1, Col2, Row2, Hordir, Verdir).

delta(X, Y, D) :- D is Y - X.


impossible_move_aux(Board, Col, Row, Col, Row, _, _) :-
    !. % The starting and ending positions are the same, so no obstruction.

impossible_move_aux(Board, Col1, Row1, Col2, Row2, Hordir, Verdir) :-
    ColNext is Col1 + Hordir,
    RowNext is Row1 + Verdir,
    position(Board, ColNext-RowNext, Piece),
    impossible_move_aux(Board, ColNext, RowNext, Col2, Row2, Hordir, Verdir).

check_move([Board, Player, _], Col1-Row1-Col2-Row2) :-
    in_bounds(Board, Col1-Row1),
    in_bounds(Board, Col2-Row2),
    valid_move(Board, Col1-Row1, Col2-Row2).

valid_move(Board, Col1-Row1, Col2-Row2) :-
    position(Board, Col1-Row1, Piece1),
    position(Board, Col2-Row2, Piece2),
    valid_direction(Piece1, Col1-Row1, Col2-Row2),
    \+ path_obstructed(Board, Col1-Row1, Col2-Row2).

move_direction(Delta, Dir) :-
    (Delta < 0 -> Dir is -1 ; Delta > 0 -> Dir is 1 ; Dir is 0).

moves_to_win(Moves, WinnerMoves) :-
    WinnerMoves is (Moves + 1) // 2.

print_winner([_, _, TotalMoves], Winner) :-
    (Winner =:= 1 -> Name = 'Player 1' ; Name = 'Player 2').
    moves_to_win(TotalMoves, WinnerMoves),
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).

game_loop(GameState) :-
    play_game_loop(GameState).

play_game_loop(GameState) :-
    display_game_state(GameState),
    print_player_turn(GameState),
    select_move(GameState, Move),
    perform_move(GameState, Move, NewGameState),
    play_game_loop(NewGameState).

print_player_turn([_, CurrentPlayer, _]) :-
    (CurrentPlayer =:= 1 -> Name = 'Player 1' ; Name = 'Player 2').
    format('~a\'s turn!\n', [Name]).

display_game_state([Board, _, _]) :-
    clean_console,
    display_game_header(1, 17),
    display_game_separator(17),
    display_game_board(Board, 1).

select_move(GameState, Col1-Row1-Col2-Row2) :-
    \+ difficulty_level(CurrentPlayer, _),
    repeat,
    get_player_move(Board, Col1-Row1-Col2-Row2),
    check_move(GameState, Col1-Row1-Col2-Row2), !.

select_move(GameState, Move) :-
    difficulty_level(CurrentPlayer, Level),
    select_best_move(GameState, CurrentPlayer, Level, Move), !.

select_best_move(GameState, CurrentPlayer, 1, Col1-Row1-Col2-Row2) :-
    valid_moves(GameState, CurrentPlayer, ListOfMoves),
    random_select_move(ListOfMoves, Col1-Row1-Col2-Row2).

select_best_move(GameState, CurrentPlayer, 2, Col1-Row1-Col2-Row2) :-
    valid_moves(GameState, CurrentPlayer, ListOfMoves),
    other_player(Player, NewPlayer),
	findall(Value-Coordinate, ( member(Coordinate, ListOfMoves), 
                                move(GameState, Coordinate, NewGameState), 
                                value(NewGameState,Player, Value1),
                                minimax(NewGameState, NewPlayer, min, 1, Value2),
                                Value is Value1 + Value2), Pairs),
    sort(Pairs, SortedPairs),
    last(SortedPairs, Max-_),
    findall(Coordinates, member(Max-Coordinates, SortedPairs), MaxCoordinates),
    random_member(ColI-RowI-ColF-RowF, MaxCoordinates).

perform_move(GameState, Col1-Row1-Col2-Row2, NewGameState) :-
    [Board, CurrentPlayer, TotalMoves] = GameState,
    position(Board, Col1-Row1, Piece),
    move_piece(Board, Col1-Row1, empty,  NewBoard1),
    move_piece(Board, Col2-Row2, piece, NewBoard2),
    switch_player(CurrentPlayer, OtherPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard2, OtherPlayer, NewTotalMoves].

play_game :-
    confs(GameState),
    game_loop(GameState),
    clear_data.
