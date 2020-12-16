:- use_module(json_client).
:- use_module(state_manipulation).



synth_dump_storable(StorableType, StorableId):-
    query_synth(dump_storable(StorableType, StorableId)).

% det: +, -
synth_load_spreadsheet(Filename, SpreadsheetId):-
    query_synth(load_spreadsheet(Filename, SpreadsheetId)).

% +, -: multi-nondet.
synth_detect_tables(SpreadsheetId, TableList, TLLength):-
    query_synth(detect_tables(SpreadsheetId, TableList)),
    length(TableList, TLLength).

% +, -, - % det for now. No alternatives to the field_types.
synth_get_field_types(TableId, FieldTypes, FieldTypeLength):-
    query_synth(get_field_types(TableId, FieldTypes)),
    length(FieldTypes, FieldTypeLength).

synth_get_incomplete_fields(TableId, NHeaderRows, IncompleteFieldList, IFLength):-
    query_synth(get_incomplete_fields(TableId, NHeaderRows, IncompleteFields)),
    findall(F/P , member(incomplete_field(F, P), IncompleteFields), IncompleteFieldList),
    length(IncompleteFieldList, IFLength).


% +, -, -, -
synth_get_field_headers(TableId, NHeaderRows, FieldHeaderList, FHLLength):-
    query_synth(get_field_headers(TableId, NHeaderRows, FieldHeaderList)),
    length(FieldHeaderList, FHLLength).


% DEPRECATED:
% % +, - (multi-nondet or just non-det?)
% synth_get_table_structure(TableId, TableStructurePreds):-
%     query_synth(get_table_structure(TableId, TableStructurePreds)).
