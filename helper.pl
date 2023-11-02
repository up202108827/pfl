:- dynamic difficulty/2.
:- dynamic nonblock/1.
:- dynamic white/1.
:- dynamic black/1.

% Define piece information and players
piece_h(black, player1, black1).
piece_h(white, player1, white1).
piece_h(nonblock, neutral).
piece_h(empty, neutral).

% Define other_player/2 predicate
other_player(player1, player2).
other_player(player2, player1).

% Define symbol/2 predicate for piece symbols
symbol(black, 'B') :- !.
symbol(white, 'W') :- !.
symbol(nonblock, '^') :- !.
symbol(empty, ' ') :- !.

% Define the game board compactly
board([
    [nonblock, nonblock, nonblock, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, nonblock, nonblock, nonblock],
    [nonblock, nonblock, nonblock, empty, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, empty, nonblock, nonblock, nonblock],
    [nonblock, nonblock, empty, nonblock, black, nonblock, empty, nonblock, black, nonblock, empty, nonblock, black, nonblock, empty, nonblock, nonblock],
    [nonblock, empty, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, empty, nonblock],
    [empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty],
    [nonblock, empty, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, empty, nonblock],
    [nonblock, nonblock, empty, nonblock, white, nonblock, empty, nonblock, white, nonblock, empty, nonblock, white, nonblock, empty, nonblock, nonblock],
    [nonblock, nonblock, nonblock, empty, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, empty, nonblock, nonblock, nonblock],
    [nonblock, nonblock, nonblock, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, nonblock, nonblock, nonblock]
]).