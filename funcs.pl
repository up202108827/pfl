
% Initialize the random number generator state
initiate_random :-
    now(X),
    setrand(X).

% Clear certain data from the knowledge base
clean_data :-
    retractall(white(_)),
    retractall(black(_)),
    retractall(difficulty(_,_)),

clean_buffer :-
    flush_output(current_input).

clean_console :-
    shell(clear, _).

% Read an integer from input
read_integer(X) :-
    read_integer_aux(X, 0).

read_integer_aux(X, Acc) :-
    get_code(Code),
    (Code >= 48, Code =< 57) , !,
    Digit is Code - 48,
    NewAcc is 10 * Acc + Digit,
    read_integer_aux(X, NewAcc).

read_integer_aux(X, X).


chose_option(Min, Max, Context, Value) :-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_integer(Value),
    (Min =< Value, Value =< Max) -> ! ; fail.

get_player_move(Board, Col1-Row1-Col2-Row2) :-
    length(Board, MaxCols),
    length(Board, MaxRows),
    get_coordinates(MaxCols, MaxRows, 'piece to move', Col1-Row1),
    get_coordinates(MaxCols, MaxRows, 'destination', Col2-Row2).
    
get_coordinates(MaxCols, MaxRows, Context, Col-Row) :-
    format('Enter the ~w column (1 to ~d): ', [Context, MaxCols]),
    get_option(1, MaxCols, '', Col),
    format('Enter the ~w row (1 to ~d): ', [Context, MaxRows]),
    get_option(1, MaxRows, '', Row).

% Read a line of input and return it as a string
read_line(Line) :-
    read_line_helper(Line, []).

replace_element_at(Index, Element, List, NewList) :-
    list_to_end(Index, List, Prefix, [_|Suffix]),
    append(Prefix, [Element|Suffix], NewList).
    
list_to_end(0, List, [], List).
list_to_end(N, [H|T], [H|Prefix], Suffix) :-
    N > 0,
    N1 is N - 1,
    list_to_end(N1, T, Prefix, Suffix).

read_line_helper(Line, Acc) :-
    get_char(Char),
    (Char = '\n' ->
        atom_chars(Line, Acc)
    ;
        append(Acc, [Char], Acc1),
        read_line_helper(Line, Acc1)
    ).






