:- use_module(synth_evaluate).
% :- use_module(synth_simulate).


action(
    load_spreadsheet(S, P, SSId),
    [data_source_path(S, P),not([spreadsheet(S, SSId)])], % You don't want it loaded twice.
    [],
    [spreadsheet(S, SSId)],
    [perform(synth_load_spreadsheet(P, SSId))]
    ).

action(
    detect_tables(SSId, TableList, TLLength), 
    [spreadsheet(_S, SSId), not([table_list(SSId, _, _)])], 
    [],
    [table_list(SSId, TableList, TLLength)],
    [perform(synth_detect_tables(SSId, TableList, TLLength))]
).

% Can't guarantee the same table doesn't get added twice from here. Do it within 
action(
    get_table(SSId, TableList, TableId), 
    [table_list(SSId, TableList, _TLLength)], 
    [],
    [table(SSId, TableId, NRows, NCols)],
    [   
        perform(member(table(SSId,TableId, NRows, NCols), TableList), _),
        perform(no_duplicate_in_state(table(SSId,TableId, NRows, NCols), State), State)
    ]
).


action(
    get_all_field_types(TableId, FieldTypeList, FTLLength),
    [ table(_S, TableId, _NRows, _NCols), not([field_type_list(TableId, _, _)])],
    [ ],
    [ field_type_list(TableId, FieldTypeList, FTLLength) ],
    [perform(synth_get_field_types(TableId, FieldTypeList, FTLLength))]
).

action(
    get_all_field_headers(TableId, FieldHeaderList, FHLLength),
    [table(_S, TableId, _NRows, _NCols), not([field_header_list(TableId, _, _, _)])],
    [],
    [field_header_list(TableId, NHeaderRows, FieldHeaderList, FHLLength)],
    [perform(synth_get_field_headers(TableId, NHeaderRows, FieldHeaderList, FHLLength))]
).

action(
    detect_tensors(TableId, FieldHeaderList, Tsr),
    [table(_S, TableId, _NRows, _NCols), field_header_list(TableId, FieldHeaderList, _)],
    [],
    [ table_tensor(TableId, Tsr) ], 
    [
        perform(synth_detect_tensors(TableId, FieldHeaderList, Tsr), _), 
        perform(no_duplicate_in_state(table_tensor(TableId, Tsr), State), State)
    ]
).
