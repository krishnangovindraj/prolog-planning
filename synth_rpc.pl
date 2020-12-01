:- use_module(json_client).

:- dynamic
    data_source/2,
    data_source_path/2. % 

% TODO: Result caching layer to not make the same call over and over.

synth_load_spreadsheet(F, S):-
    query_synth(load_spreadsheet(F, S)).

synth_detect_tables(S,T):-
    query_synth(detect_tables(S, T)).

synth_get_field_types(T, FT):-
    query_synth(get_field_types(T,FT)).
