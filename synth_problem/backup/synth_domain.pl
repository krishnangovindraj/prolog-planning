:- use_module(synth_evaluate).
% :- use_module(synth_simulate).


action(
    load_spreadsheet(S),
    [data_source_path(S, P),not(spreadsheet(S)), evaluate(synth_load_spreadsheet(P, S))], % You don't want it loaded twice.
    [],
    [spreadsheet(S)]
    ).

action(
    detect_tables(S, TableList, TLLength), 
    [spreadsheet(S), not(table_list(S, _, _)), evaluate(synth_detect_tables(S, TableList, TLLength))], 
    [],
    [table_list(S, TableList, TLLength)]
).


action(
    get_table(S, TableId), 
    [table_list(S, TableList, TLLength), evaluate(synth_member(table(TableId, NRows, NCols), TableList, TLLength)), not(table(TableId, NRows, NCols))], 
    [],
    [table(TableId, NRows, NCols)]
).


action(
    get_all_field_types(TableId, FieldTypeList, FTLLength),
    [ table(_S, TableId), not(field_type_list(TableId, _, _)), evaluate(synth_get_field_types(TableId, FieldTypeList, FTLLength)) ],
    [ ],
    [ field_type_list(TableId, FieldTypeList, FTLLength) ] % This might get confusing since FT is a compound field_type(X,Y,Z)
).

action(
    get_all_field_headers(TableId, FieldHeaderList, FHLLength),
    [table(_S, TableId), not(field_header_list(TableId, _, _, _)), synth_get_field_headers(TableId, NHeaderRows, FieldHeaderList, FHLLength)],
    [],
    [field_header_list(TableId, NHeaderRows, FieldHeaderList, FHLLength)] 
).

action(
    detect_tensors(TableId, Tsr),
    [table(_S, TableId), field_header_list(TableId, FieldHeaderList, _), evaluate(synth_detect_tensors(TableId, FieldHeaderList, Tsr)), not(table_tensor(TableId, Tsr))],
    [],
    [ table_tensor(TableId, Tsr) ] % TODO: Find better way to prevent ourselves adding the same tensor over and again
).

