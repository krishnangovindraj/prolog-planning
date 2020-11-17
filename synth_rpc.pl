:- use_module(json_client).

:- dynamic
    data_source/2,
    data_source_path/2. % 

% TODO: Result caching layer to not make the same call over and over.

synth_load_spreadsheet(S):-
    data_source_path(S, F),
    query_synth(load_spreadsheet(S, F)).

synth_detect_table(S,T):-
    data_source_path(S, F),
    query_synth(detect_tables(S, T, F)).
