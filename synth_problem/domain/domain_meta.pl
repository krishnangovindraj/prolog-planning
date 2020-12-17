
% Section 1: Load tables & get meta-data
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
    get_table(SSId, TableList, TableId, NRows, NCols), 
    [table_list(SSId, TableList, _TLLength)], 
    [],
    [table(SSId, TableId, NRows, NCols)],
    [   
        perform(member(table(SSId,TableId, NRows, NCols), TableList), _),
        perform(no_duplicate_in_state(table(SSId,TableId, NRows, NCols), State), State)
    ]
).

action(
    get_table_title(TableId, Title),
    [table(_SSId, TableId, _, _ ), not([table_title(TableId, Title)])],
    [],
    [table_title(TableId, Title)],
    [perform(synth_get_table_cell(TableId, 0, 0, Title))]
).


action(
    get_all_field_types(TableId, FieldTypeList, FTLLength),
    [ table(_S, TableId, _NRows, _NCols), not([field_type_list(TableId, _, _)])],
    [ ],
    [ field_type_list(TableId, FieldTypeList, FTLLength) ],
    [perform(synth_get_field_types(TableId, FieldTypeList, FTLLength))]
).


action(
    get_incomplete_fields(TableId, NHeaderRows, IncompleteFieldList, IFLLength),
    [ table(_S, TableId, _NRows, _NCols), not([incomplete_field_list(TableId, _, _)]),
        field_header_list(TableId, NHeaderRows, _, _) ],
    [ ],
    [ incomplete_field_list(TableId, IncompleteFieldList, IFLLength) ],
    [perform(synth_get_incomplete_fields(TableId, NHeaderRows, IncompleteFieldList, IFLLength))]
).


action(
    get_all_field_headers(TableId, NHeaderRows, FieldHeaderList, FHLLength),
    [table(_S, TableId, _NRows, _NCols), not([field_header_list(TableId, _, _, _)])],
    [],
    [field_header_list(TableId, NHeaderRows, FieldHeaderList, FHLLength)],
    [perform(synth_get_field_headers(TableId, NHeaderRows, FieldHeaderList, FHLLength))]
).

% Easy goal so I don't have to do the steps.
action(
    checkpoint_table_all_meta(TId),
    [ table(_, TId, _, _), not([all_meta_loaded(TId)]), 
        field_header_list(TId, _, _, _), field_type_list(TId,_,_), incomplete_field_list( TId, _, _)
    ],
    [],
    [all_meta_loaded(TId)],
    []
).
