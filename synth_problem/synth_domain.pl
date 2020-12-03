:- consult(synth_evaluate).
:- consult(synth_simulate).


action(
    load_spreadsheet(S),
    [data_source_path(S, P),not(spreadsheet(S)), evaluate(synth_load_spreadsheet(P, S))], % You don't want it loaded twice.
    [],
    [spreadsheet(S)]
    ).

% action(
%     detect_tables(S,T), 
%     [spreadsheet(S), evaluate(synth_detect_table(S,T))], % not(table(S,T)) can be used if the tables are all added at once.
%     [],
%     [table(S, T)]
% ).


action(
    detect_all_tables(S,TableList), 
    [spreadsheet(S), not(all_tables_detected(S)), evaluate(synth_detect_table(S,TableList))], 
    [],
    [all_tables_detected(S)| TableList]
).

action(
    get_field_types(T, FieldTypeList),
    [ table(S,T),   ],
    [ ],
    [ field_type(FT) ] % This might get confusing since FT is a compound field_type(X,Y,Z)
)

action(
    detect_tensors(Tbl, Tsr),
    [table(S,Tbl), evaluate(synth_detect_tensors(Tbl, Tsr))],
    [],
    [ tensor(Tbl, Tsr) ]    
)