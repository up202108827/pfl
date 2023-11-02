:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(confs).
:- consult(board).


valid_moves([Board, Player, _], Player, ValidMoves) :-
    findall(Col1-Row1-Col2-Row2, (
        member(Col1-Row1, Board),
        position(Board, Col1-Row1, Player), % Check if the piece belongs to the current player
        move_directions(Directions), % Define the possible move directions
        member(Direction, Directions),
        is_valid_move([Board, Player, _], Col1-Row1, Direction, Col2-Row2)
    ), ValidMoves).

% Define the move_directions predicate to specify the possible move directions
move_directions([up, down, left_up, left_down, right_up, right_down]).

% Define the is_valid_move predicate to check if a move is valid
is_valid_move([Board, _, _], Col1-Row1, Direction, Col2-Row2) :-
    next_position(Col1-Row1, Direction, Col2-Row2),
    in_bounds(Board, Col2-Row2), % Check if the move is within the board bounds
    position(Board, Col2-Row2, empty), % Check if the target position is empty
    path_clear(Board, Col1-Row1, Col2-Row2), % Check if the path is clear (no pieces in between)
    not_in_goal(Col2-Row2). % Check if the target position is not the opponents goal

% Define the next_position predicate to calculate the next position based on the direction
next_position(Col1-Row1, up, Col2-Row2) :-
    Col2 is Col1,
    Row2 is Row1 - 1.
next_position(Col1-Row1, down, Col2-Row2) :-
    Col2 is Col1,
    Row2 is Row1 + 1.
next_position(Col1-Row1, left_up, Col2-Row2) :-
    Col2 is Col1 - 1,
    Row2 is Row1.
next_position(Col1-Row1, left_down, Col2-Row2) :-
    Col2 is Col1 - 1,
    Row2 is Row1 + 1.
next_position(Col1-Row1, right_up, Col2-Row2) :-
    Col2 is Col1 + 1,
    Row2 is Row1 - 1.
next_position(Col1-Row1, right_down, Col2-Row2) :-
    Col2 is Col1 + 1,
    Row2 is Row1.


% Define path_clear predicate to check if the path between two positions is clear
path_clear(Board, Col1-Row1, Col2-Row2) :-
    path_obstructed(Board, Col1-Row1, Col2-Row2).

% Define path_obstructed predicate to check if there are pieces in the path between two positions
path_obstructed(Board, Col1-Row1, Col2-Row2) :-
    move_direction(DeltaCol, DeltaRow, Col1-Row1, Col2-Row2),
    path_obstructed_aux(Board, Col1, Row1, Col2, Row2, DeltaCol, DeltaRow).

path_obstructed_aux(_, Col1, Row1, Col2, Row2, _, _) :-
    % Base case: When the two positions are the same, there is no obstruction.
    Col1 =:= Col2,
    Row1 =:= Row2.

path_obstructed_aux(Board, Col1, Row1, Col2, Row2, DeltaCol, DeltaRow) :-
    % Recursive case: Check if there is a piece at the next position along the path.
    ColNext is Col1 + DeltaCol,
    RowNext is Row1 + DeltaRow,
    position(Board, ColNext-RowNext, Piece),
    Piece \= empty,
    path_obstructed_aux(Board, ColNext, RowNext, Col2, Row2, DeltaCol, DeltaRow).

% Define move_direction predicate to calculate the direction of movement
move_direction(DeltaCol, DeltaRow, Col1-Row1, Col2-Row2) :-
    DeltaCol is abs(Col2 - Col1),
    DeltaRow is abs(Row2 - Row1),
    (DeltaCol =:= 1 ; DeltaRow =:= 1 ; (DeltaCol =:= DeltaRow, DeltaCol =\= 0)).

% Define not_in_goal predicate to check if a position is not the opponents goal
not_in_goal(Col-Row) :- % Define the conditions to check if a position is not in the opponents goal.
    % You may need to specify the rules for the goal positions based on the board layout.
    % For example, if the goal is in the last row for player1 and the first row for player2:
    Player == player1,
    Row \= 1.

not_in_goal(Col-Row) :- % Define the conditions for player2 goal.
    Player == player2,
    Row \= MaxRows.

get_direction(Col1-Row1-Col2-Row2, Direction) :-
    DeltaCol is Col2 - Col1,
    DeltaRow is Row2 - Row1,
    (DeltaCol =:= 0, DeltaRow =:= -1, Direction = up;
     DeltaCol =:= 0, DeltaRow =:= 1, Direction = down;
     DeltaCol =:= -1, DeltaRow =:= 0, Direction = left;
     DeltaCol =:= -1, DeltaRow =:= 1, Direction = left_down;
     DeltaCol =:= 1, DeltaRow =:= -1, Direction = right_up;
     DeltaCol =:= 1, DeltaRow =:= 0, Direction = right).

print_winner([_, _, TotalMoves], Winner) :-
    (Winner == 'player1' -> Name = 'Player 1' ; Name = 'Player 2'),
    FinalMoves is (TotalMoves + 1) // 2,
    format('Winner is ~a with ~d moves!\n', [Name, FinalMoves]).

game_loop(GameState) :-
    play_game_loop(GameState).

play_game_loop(GameState) :-
    display_game(GameState),
    print_player_turn(GameState),
    choose_move(GameState, Move),
    write('aaa'),
    move(GameState, Move, NewGameState),
    play_game_loop(NewGameState).

print_player_turn([_, CurrentPlayer, _]) :-
    (CurrentPlayer == 'player1' -> Name = 'Player 1' ; Name = 'Player 2'),
    format('~a\'s turn!\n', [Name]).

display_game([Board, _, _]) :-
    clean_console,
    display_game_header(1, 17),
    display_game_separator(17),
    display_game_board(Board, 1), !.

get_player_move(Board, Col1, Row1, Col2, Row2) :-
    write('Choose your move (e.g., Col1-Row1-Col2-Row2): '),
    read(NextMove),
    split_string(NextMove, "-", "", [Col1Str, Row1Str, Col2Str, Row2Str]),
    atom_number(Col1Str, Col1),
    atom_number(Row1Str, Row1),
    atom_number(Col2Str, Col2),
    atom_number(Row2Str, Row2).


choose_move(GameState, Col1-Row1-Col2-Row2) :-
    write('BBB'),
    \+ difficulty(CurrentPlayer, _),
    repeat,
    get_player_move(Board, Col1, Row1, Col2, Row2),
    write('ccc'),
    get_direction(Col1-Row1-Col2-Row2, Direction),
    is_valid_move(GameState, Col1-Row1, Direction, Col2-Row2), !.

choose_move(GameState, Move) :-
    write('ddd'),
    difficulty(CurrentPlayer, Level),
    choose_move(GameState, CurrentPlayer, Level, Move), !.

choose_move(GameState, CurrentPlayer, 1, Col1-Row1-Col2-Row2) :-
    write('eee'),
    valid_moves(GameState, CurrentPlayer, ListOfMoves),
    random_member(Move, ListOfMoves).

choose_move(GameState, CurrentPlayer, 2, Col1-Row1-Col2-Row2) :-    
    write('fff'),
    valid_moves(GameState, CurrentPlayer, ListOfMoves),
    other_player(Player, NewPlayer),
    write('ggg'),
	findall(Value-Coordinate, ( member(Coordinate, ListOfMoves), 
                                move(GameState, Coordinate, NewGameState), 
                                value(NewGameState,Player, Value1),
                                minimax(NewGameState, NewPlayer, min, 1, Value2),
                                Value is Value1 + Value2), Pairs),
    sort(Pairs, SortedPairs),
    last(SortedPairs, Max-_),
    write('hhh'),
    findall(Coordinates, member(Max-Coordinates, SortedPairs), MaxCoordinates),
    random_member(ColI-RowI-ColF-RowF, MaxCoordinates).

move(GameState, Col1-Row1-Col2-Row2, NewGameState) :-
    [Board, CurrentPlayer, TotalMoves] = GameState,
    position(Board, Col1-Row1, Piece),
    move_piece(Board, Col1-Row1, empty,  NewBoard1),
    move_piece(Board, Col2-Row2, piece, NewBoard2),
    other_player(CurrentPlayer, OtherPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard2, OtherPlayer, NewTotalMoves].


% Define the value predicate (simple piece count as the heuristic)
value([Board, Player, _], Player, Value) :-
    count_pieces(Board, Player, Count),
    Value is Count.

% Define the minimax predicate
minimax(GameState, Player, max, 0, Value) :-
    value(GameState, Player, Value).

minimax(GameState, Player, min, 0, Value) :-
    other_player(Player, Opponent),
    value(GameState, Opponent, Value).

minimax(GameState, Player, max, Depth, BestValue) :-
    valid_moves(GameState, Player, Moves),
    max_value(GameState, Player, Depth, Moves, -9999, 9999, BestValue).

minimax(GameState, Player, min, Depth, BestValue) :-
    valid_moves(GameState, Player, Moves),
    min_value(GameState, Player, Depth, Moves, -9999, 9999, BestValue).

max_value(_, _, 0, _, _, _, 0).

max_value(GameState, Player, Depth, [Move | Moves], Alpha, Beta, Value) :-
    move(GameState, Move, NewGameState),
    next_player(Player, Opponent),
    minimax(NewGameState, Opponent, min, Depth - 1, MinValue),
    NewAlpha is max(Alpha, MinValue),
    (NewAlpha >= Beta ->
        Value = NewAlpha
    ;
        max_value(GameState, Player, Depth, Moves, NewAlpha, Beta, Value)
    ).

min_value(_, _, 0, _, _, _, 0).

min_value(GameState, Player, Depth, [Move | Moves], Alpha, Beta, Value) :-
    move(GameState, Move, NewGameState),
    next_player(Player, Opponent),
    minimax(NewGameState, Opponent, max, Depth - 1, MaxValue),
    NewBeta is min(Beta, MaxValue),
    (Alpha >= NewBeta ->
        Value = NewBeta
    ;
        min_value(GameState, Player, Depth, Moves, Alpha, NewBeta, Value)
    ).

% Define the count_pieces predicate
count_pieces(Board, Player, Count) :-
    findall(_, (member(Col-Row, Board), position(Board, Col-Row, Player)), Pieces),
    length(Pieces, Count).


play :-
    confs(GameState),
    game_loop(GameState),
    clean_data.
