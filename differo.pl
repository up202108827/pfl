
% Entry point for the game
:- initialization(play).

% Define game types
game_type(hh).
game_type(hpc).
game_type(pch).
game_type(pcpc).

% Define difficulty levels
difficulty(easy).
difficulty(medium).
difficulty(hard).

% Main predicate to start the game menu
play :-
    write('Welcome to Differo! Choose a game type:'), nl,
    write('1. Human vs. Human (HH)'), nl,
    write('2. Human vs. AI (HPC)'), nl,
    write('3. AI vs. Human (PCH)'), nl,
    write('4. AI vs. AI (PCPC)'), nl,
    read(GameChoice),
    select_game_type(GameChoice).

% Predicate to handle game type selection
select_game_type(1) :-
    game_type(hh),
    choose_difficulty(Player1Difficulty),
    choose_difficulty(Player2Difficulty),
    start_game(hh, Player1Difficulty, Player2Difficulty).

select_game_type(2) :-
    game_type(hpc),
    choose_difficulty(PlayerDifficulty),
    start_game(hpc, PlayerDifficulty, _).

select_game_type(3) :-
    game_type(pch),
    choose_difficulty(PlayerDifficulty),
    start_game(pch, _, PlayerDifficulty).

select_game_type(4) :-
    game_type(pcpc),
    choose_difficulty(Player1Difficulty),
    choose_difficulty(Player2Difficulty),
    start_game(pcpc, Player1Difficulty, Player2Difficulty).

% Predicate to choose difficulty
choose_difficulty(Difficulty) :-
    write('Choose a difficulty level:'), nl,
    write('1. Easy'), nl,
    write('2. Medium'), nl,
    write('3. Hard'), nl,
    read(DifficultyChoice),
    difficulty_choice(DifficultyChoice, Difficulty).

difficulty_choice(1, easy).
difficulty_choice(2, medium).
difficulty_choice(3, hard).

% Predicate to start the game
start_game(GameType, Player1Difficulty, Player2Difficulty) :-
    write('Starting the game...'), nl,
    % Here, you would implement the actual game logic based on the chosen parameters.

% Predicate to start the game (HH version)
start_game(hh, _, _) :-
    % Initialize the game board
    initialize_board(Board),
    % Start the game loop
    game_loop(hh, Board).

% Initialize the game board (3x3 board with empty cells)
initialize_board([
    [' ', ' ', ' '],
    [' ', ' ', ' '],
    [' ', ' ', ' ']
]).

% Game loop
game_loop(Player, Board) :-
    display_board(Board),
    % Check for a win condition or a draw
    (check_win(Board, Player) ->
        write(Player), write(' wins!'), nl
    ; board_full(Board) ->
        write('It\'s a draw!'), nl
    ;
        % Continue the game
        (Player == 'X' -> NextPlayer = 'O' ; NextPlayer = 'X'),
        write(Player), write('\'s turn. Enter row and column (e.g., 2,3): '),
        read(Row-Col),
        make_move(Row, Col, Player, Board, NewBoard),
        game_loop(NextPlayer, NewBoard)
    ).

% Display the game board
display_board(Board) :-
    nl,
    write('   1   2   3  '), nl,
    write('1  '), display_row(Board, 1), nl,
    write('   ---------  '), nl,
    write('2  '), display_row(Board, 2), nl,
    write('   ---------  '), nl,
    write('3  '), display_row(Board, 3), nl.

display_row([Cell|Rest], Row) :-
    write(Cell), write(' | '),
    display_row(Rest, Row).
display_row([], _) :-
    nl.

% Make a move on the board
make_move(Row, Col, Player, Board, NewBoard) :-
    nth1(Row, Board, OldRow),
    nth1(Col, OldRow, Cell),
    Cell == ' ',
    replace(OldRow, Col, Player, NewRow),
    replace(Board, Row, NewRow, NewBoard).

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

% Check for a win condition
check_win(Board, Player) :-
    % You would implement your win condition logic here.
    % For simplicity, lets assume a win is forming a line of your symbol (Player) in any direction.

% Check if the board is full
board_full(Board) :-
    flatten(Board, Flattened),
    \+ member(' ', Flattened).







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
    Piece = ' ',
    impossible_move_aux(Board, ColNext, RowNext, Col2, Row2, Hordir, Verdir).

check_move([Board, Player, _], Col1-Row1-Col2-Row2) :-
    in_bounds(Board, Col1-Row1),
    in_bounds(Board, Col2-Row2),
    valid_move(Board, Col1-Row1, Col2-Row2),
    \+ path_obstructed(Board, Col1-Row1, Col2-Row2).

valid_move(Board, Col1-Row1, Col2-Row2) :-
    position(Board, Col1-Row1, Piece1),
    position(Board, Col2-Row2, Piece2),
    Piece1 \= ' ', Piece2 \= ' ',
    valid_direction(Piece1, Col1-Row1, Col2-Row2).

move_direction(Delta, Dir) :-
    (Delta < 0 -> Dir is -1 ; Delta > 0 -> Dir is 1 ; Dir is 0).

moves_to_win(Moves, WinnerMoves) :-
    WinnerMoves is (Moves + 1) // 2.

print_winner([_, _, TotalMoves], Winner) :-
    (Winner =:= 1 -> Name = 'Player 1' ; Name = 'Player 2').
    moves_to_win(TotalMoves, WinnerMoves),
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).

play_game(GameState) :-
    play_game_loop(GameState).

play_game_loop(GameState) :-
    display_game_state(GameState),
    print_player_turn(GameState),
    choose_next_move(GameState, Move),
    make_move(GameState, Move, NewGameState),
    play_game_loop(NewGameState).

print_player_turn([_, CurrentPlayer, _]) :-
    (CurrentPlayer =:= 1 -> Name = 'Player 1' ; Name = 'Player 2').
    format('~a\'s turn!\n', [Name]).

display_game_state([Board, _, _]) :-
    clear_console,
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
    setup_game(GameState),
    game_loop(GameState),
    clear_data.
