:- use_module(library(lists)).
:- consult(funcs).
:- consult(helper).



% Main predicate to start the game menu
play :-
    write('Welcome to Differo! Choose a game type:\n'), nl,
    write('1. Human vs. Human \n'), nl,
    write('2. Human vs. PC \n'), nl,
    write('3. PC vs. PC \n'), nl,
    read(GameChoice),
    option(GameChoice).

% Predicate to choose difficulty
choose_difficulty(Player) :-
    write('Choose a difficulty level:\n'), nl,
    write('1. Easy\n'), nl,
    write('2. Medium\n'), nl,
    write('3. Hard\n'), nl,
    read(DifficultyChoice),
    asserta(difficulty(Player, DifficultyChoice)).


option(2):- 
    choose_difficulty(player2),
    choose_difficulty(player2).

option(3):-
    choose_difficulty(player1),
    choose_difficulty(player2),
    choose_difficulty(player1),
    choose_difficulty(player2).

first_player(Player):-
    format('Who starts playing?\n1 - ~a \n2 - ~a \n', ['Player 1', 'Player 2']),
    read(Index),
    nth1(Index, [player1, player2], Player).

confs([Board, Player, 0]) :- 
        play,
        initiate_random,
        first_player(Player),
        initialize_board(Board).
