


% Clear certain data from the knowledge base
clean_data :-
    retractall(white(_)),
    retractall(black(_)),
    retractall(difficulty(_,_)).

clean_buffer :-
    flush_output(current_input).

clean_console :-
    write('\33\[2J').








