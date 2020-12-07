:- consult(synth_evaluate).
:- consult(synth_simulate).


action(
    load_spreadsheet(S),
    [data_source_path(S, P),not(spreadsheet(S)), evaluate(synth_load_spreadsheet(P, S))], % You don't want it loaded twice.
    [],
    [spreadsheet(S)]
    ).

action(
    detect_tables(S,TableList), 
    [spreadsheet(S), not(done_detect_tables(S)), evaluate(synth_detect_tables(S,TableList))], 
    [],
    [done_detect_tables(S)| TableList]
).

action(
    get_field_types(T, FieldTypeList),
    [ table(S,T), not(done_get_field_types(T)), evaluate(synth_get_field_types(T, FieldTypeList)) ],
    [ ],
    [ done_get_field_types(T) | FieldTypeList ] % This might get confusing since FT is a compound field_type(X,Y,Z)
).

action(
    detect_tensors(Tbl, Tsr),
    [table(S,Tbl), evaluate(synth_detect_tensors(Tbl, Tsr))],
    [],
    [ tensor(Tbl, Tsr) ]    
).