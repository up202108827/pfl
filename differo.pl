:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(confs).
:- consult(board).


valid_moves([Board, Player, _, _, _], Player, ValidMoves, [], _).
valid_moves([Board, Player, _, _, _], Player, ValidMoves, _, []).

valid_moves([Board, Player, _,_ ,_], Player, ValidMoves, [HeadWhite|TailWhite], [HeadBlack|TailBlack]) :-
    write('lll1\n'),
    (Player =:= player1 -> 
        (write('lll2\n'),
        % count_diagonal_pieces(Board, HeadWhite, BlackCount1, WhiteCount1, BlackCount2, WhiteCount2, BlackCount3, WhiteCount3),
        Up_Left is WhiteCount1 - BlackCount1,
        Up_Right is WhiteCount2 - BlackCount2,
        Horizontal is WhiteCount3 - BlackCount3,
        write(Up_Left),
        write(Up_Right),
        write(Horizontal),
        next_position(HeadWhite, left, Value, Col2-Row2),
        is_valid_move(GameState, HeadWhite, left, Col2-Row2) -> append([HeadWhite-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadWhite, right, Value, Col2-Row2),
        is_valid_move(GameState, HeadWhite, right, Col2-Row2) -> append([HeadWhite-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadWhite, left_up, Value, Col2-Row2),
        is_valid_move(GameState, HeadWhite, left_up, Col2-Row2) -> append([HeadWhite-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadWhite, left_down, Value, Col2-Row2),
        is_valid_move(GameState, HeadWhite, left_down, Col2-Row2) -> append([HeadWhite-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadWhite, right_up, Value, Col2-Row2),
        is_valid_move(GameState, HeadWhite, right_up, Col2-Row2) -> append([HeadWhite-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadWhite, right_down, Value, Col2-Row2),
        is_valid_move(GameState, HeadWhite, right_down, Col2-Row2) -> append([HeadWhite-Col2-Row2], ValidMoves, FinalMoves),
        valid_moves([Board, Player, _, _, _], Player, FinalMoves, TailWhite, [HeadBlack|TailBlack])
        );
        (write('lll3\n'),
        count_diagonal_pieces(Board, HeadBlack, BlackCount1, WhiteCount1, BlackCount2, WhiteCount2, BlackCount3, WhiteCount3),
        Up_Left is BlackCount1 - WhiteCount1,
        Up_Right is BlackCount2 - WhiteCount2,
        Horizontal is BlackCount3 - WhiteCount3,
        next_position(HeadBlack, left, Value, Col2-Row2),
        is_valid_move(GameState, HeadBlack, left, Col2-Row2) -> append([HeadBlack-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadBlack, right, Value, Col2-Row2),
        is_valid_move(GameState, HeadBlack, right, Col2-Row2) -> append([HeadBlack-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadBlack, left_up, Value, Col2-Row2),
        is_valid_move(GameState, HeadBlack, left_up, Col2-Row2) -> append([HeadBlack-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadBlack, left_down, Value, Col2-Row2),
        is_valid_move(GameState, HeadBlack, left_down, Col2-Row2) -> append([HeadBlack-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadBlack, right_up, Value, Col2-Row2),
        is_valid_move(GameState, HeadBlack, right_up, Col2-Row2) -> append([HeadBlack-Col2-Row2], ValidMoves, FinalMoves),
        next_position(HeadBlack, right_down, Value, Col2-Row2),
        is_valid_move(GameState, HeadBlack, right_down, Col2-Row2) -> append([HeadBlack-Col2-Row2], ValidMoves, FinalMoves),
        valid_moves([Board, Player, _, _, _], Player, FinalMoves, [HeadWhite|TailWhite], TailBlack)
        )
    ).
    


% Define the move_directions predicate to specify the possible move directions
move_directions([left, right, left_up, left_down, right_up, right_down]).


% Define the is_valid_move predicate to check if a move is valid
is_valid_move([Board, Player, _, _, _], Col1-Row1, Direction, Col2-Row2) :-
    write('jjj'),
    position(Board, Col2-Row2, Piece),
    (Piece =:= empty -> true;
    (Player =:= player1 ->
        (position(Board, Col2-Row2, Piece) = wgoal ->
            game_over(player1)
        ; false)
    ; Player =:= player2 ->
        (position(Board, Col2-Row2, Piece) = bgoal ->
            game_over(player2)
        ; false)
    )
    ; false).

% Define the next_position predicate to calculate the next position based on the direction
next_position(Col1-Row1, left, Value, Col2-Row2) :-
    (Value =< 0 -> (Col2 is Col1, Row2 is Row1), !);
    Col2 is Col1 - Value,
    Row2 is Row1.
next_position(Col1-Row1, right, Value, Col2-Row2) :-
    (Value =< 0 -> (Col2 is Col1, Row2 is Row1), !);
    Col2 is Col1 + Value,
    Row2 is Row1.
next_position(Col1-Row1, left_up, Value, Col2-Row2) :-
    (Value =< 0 -> (Col2 is Col1, Row2 is Row1), !);
    Col2 is Col1 - Value,
    Row2 is Row1 - Value.
next_position(Col1-Row1, left_down, Value, Col2-Row2) :-
    (Value =< 0 -> (Col2 is Col1, Row2 is Row1), !);
    Col2 is Col1 - Value,
    Row2 is Row1 + Value.
next_position(Col1-Row1, right_up, Value, Col2-Row2) :-
    (Value =< 0 -> (Col2 is Col1, Row2 is Row1), !);
    Col2 is Col1 + Value,
    Row2 is Row1 - Value.
next_position(Col1-Row1, right_down, Value, Col2-Row2) :-
    (Value =< 0 -> (Col2 is Col1, Row2 is Row1), !);
    Col2 is Col1 + Value,
    Row2 is Row1 + Value.


% Count black and white pieces on both diagonals for a given piece
count_diagonal_pieces(Board, Col-Row, BlackCount1, WhiteCount1, BlackCount2, WhiteCount2, BlackCount3, WhiteCount3) :-
    write('Count pieces 2\n'), 
    count_pieces_on_diagonal(Board, Col-Row, up_left, BlackCount1, WhiteCount1),
    count_pieces_on_diagonal(Board, Col-Row, up_right, BlackCount2, WhiteCount2),
    count_pieces_on_diagonal(Board, Col-Row, horizontal, BlackCount3, WhiteCount3).


% Count pieces on a single diagonal
count_pieces_on_diagonal(Board, Col-Row, Direction, BlackCount, WhiteCount) :-
    write('Count pieces 1\n'),   
    (Direction = up_left ->
        count_pieces_on_diagonal_aux(Board, Col, Row, -1, -1, BlackCount1, WhiteCount1),
        count_pieces_on_diagonal_aux(Board, Col, Row, 1, 1, BlackCount2, WhiteCount2);
    Direction = up_right ->
        count_pieces_on_diagonal_aux(Board, Col, Row, 1, -1, BlackCount1, WhiteCount1),
        count_pieces_on_diagonal_aux(Board, Col, Row, -1, 1, BlackCount2, WhiteCount2);
        
        count_pieces_on_diagonal_aux(Board, Col, Row, 1, 0, BlackCount1, WhiteCount1),
        count_pieces_on_diagonal_aux(Board, Col, Row, -1, 0, BlackCount2, WhiteCount2)),
    BlackCount is BlackCount1 + BlackCount2,
    WhiteCount is WhiteCount1 + WhiteCount2, !.

% Base case: No more steps to count
count_pieces_on_diagonal_aux(_, 0, _, _, _, _, _). 
count_pieces_on_diagonal_aux(_, 18, _, _, _, _, _).
count_pieces_on_diagonal_aux(_, _, 0, _, _, _, _).
count_pieces_on_diagonal_aux(_, _, 10, _, _, _, _).

count_pieces_on_diagonal_aux(Board, Col, Row, DeltaCol, DeltaRow, BlackCount, WhiteCount) :-
    write('Count pieces \n'),
    Col2 is Col + DeltaCol,
    Row2 is Row + DeltaRow,
    position(Board, Col2-Row2, Piece),
    (Piece = nonblock -> B is 0, !);
    (Piece = black -> BlackCount1 is 1, WhiteCount1 is 0; Piece = white -> BlackCount1 is 0, WhiteCount1 is 1; BlackCount1 is 0, WhiteCount1 is 0),
    BlackCount2 is BlackCount1 + BlackCount,
    WhiteCount2 is WhiteCount1 + WhiteCount,
    count_pieces_on_diagonal_aux(Board, Col2, Row2, DeltaCol, DeltaRow, BlackCount2, WhiteCount2).



game_over([_, _, TotalMoves, _, _], Winner) :-
    (Winner == 'player1' -> Name = 'Player 1' ; Name = 'Player 2'),
    FinalMoves is (TotalMoves + 1) // 2,
    format('Winner is ~a with ~d moves!\n', [Name, FinalMoves]).

game_loop(GameState, WhitePieces, BlackPieces) :-
    play_game_loop(GameState, WhitePieces, BlackPieces).

play_game_loop(GameState, WhitePieces, BlackPieces) :-
    clean_lists(GameState),
    write('write display\n'),
    display_game(GameState),
    print_player_turn(GameState),
    choose_way(GameState, Move, WhitePieces, BlackPieces),
    write('aaa'),
    write(Move), write('\n'), 
    move(GameState, Move, NewGameState),
    write('aaa'),
    play_game_loop(NewGameState).

clean_lists([ _, _, _, [], []]).

print_player_turn([_, CurrentPlayer, _, _, _]) :-
    (CurrentPlayer == 'player1' -> Name = 'Player 1' ; Name = 'Player 2'),
    format('~a\'s turn!\n', [Name]).

display_game([Board, _, _, WhitePieces, BlackPieces]) :-
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

% random_member/2: Select a random member from a list
random_member(Member, List) :-
    length(List, Length),
    random(0, Length, Index),
    nth0(Index, List, Member).

% Define a predicate to print a list of moves
print_moves([]).
print_moves([Move|Rest]) :-
    format('List: ~w\n', [Move]),
    print_moves(Rest).



choose_way([Board, Player, _, _ , _], Move, WhitePieces, BlackPieces) :-
    length(BlackPieces, L1), length(WhitePieces, L2),
    write(L1), write(L2),
    write('Tamanho choose_way\n'),
    (difficulty(Player, 0) ->
        (get_player_move(Board, Col1, Row1, Col2, Row2),
         write('ccc'),
         is_valid_move(GameState, Col1-Row1, Direction, Col2-Row2), 
         Move = Col1-Row1-Col2-Row2, !)
    ;
    choose_move(GameState, Player, Level, Move, WhitePieces, BlackPieces), !).

choose_move([Board, Player, _, _, _], Player, 1, Move, WhitePieces, BlackPieces) :-
    write('eee'),
    valid_moves([Board, Player, _, _, _], Player, ListOfMoves, WhitePieces, BlackPieces),
    random_member(Move, ListOfMoves),
    length(BlackPieces, Length),
    length(WhitePieces, Length1),
    write(Length),
    write(Length1),
    write(Player),
    write('Tamanho choose_move\n'),
     !.

choose_move([Board, Player, _, WhitePieces, BlackPieces], Player, 2, Col1-Row1-Col2-Row2) :-    
    write('fff'),
    valid_moves(GameState, Player, ListOfMoves),
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
    random_member(ColI-RowI-ColF-RowF, MaxCoordinates), !.

move([Board, Player, TotalMoves, _, _], Col1-Row1-Col2-Row2, NewGameState) :-
    position(Board, Col1-Row1, Piece),
    move_piece(Board, Col1-Row1, empty,  NewBoard1),
    write(Col2),
    write(Row2),
    move_piece(Board, Col2-Row2, piece, NewBoard2),
    write('AAAA'),
    other_player(Player, OtherPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard2, OtherPlayer, NewTotalMoves, _, _].


% Define the value predicate (simple piece count as the heuristic)
value([Board, Player, _, _, _], Player, Value) :-
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
    write('play 2'),
    extract_pieces(GameState, WhitePieces, BlackPieces),
    length(BlackPieces, L1), length(WhitePieces, L2),
    write(L1), write(L2), write('play'),
    game_loop(GameState, WhitePieces, BlackPieces),
    clean_data.
