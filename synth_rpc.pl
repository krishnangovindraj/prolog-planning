:- use_module(json_client).

:- dynamic
    data_source/2,
    data_source_path/2. % 

% TODO: Result caching layer to not make the same call over and over.
% TODO: Better argument names

% det: +, -
synth_load_spreadsheet(Filename, SpreadsheetId):-
    query_synth(load_spreadsheet(Filename, SpreadsheetId)).

% nondet: +, -
synth_detect_tables(SpreadsheetId,TableId):-
    query_synth(detect_tables(SpreadsheetId, TableId)).

% nondet: +, -
synth_get_field_types(TableId, FieldTypes):-
    query_synth(get_field_types(TableId,FieldTypes)).

% nondet: +, -
synth_get_table_structure(TableId, TableStructurePreds):-
    query_synth(get_table_structure(TableId, TableStructurePreds)).
