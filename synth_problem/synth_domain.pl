:- consult(synth_evaluate).
:- consult(synth_simulate).
% From the pddl: 
%%  (:types table sheet model - object
%%          countor-constraint tacle-constraint segmentation dreaml mercs admercs lgg-rule - model)

action(
    load_spreadsheet(S),
    [data_source_path(S, P), evaluate(synth_load_spreadsheet(P, S))], % I can add something 
    [],
    [spreadsheet(S)]
    ).

action(
    detect_tables(S,T), 
    [spreadsheet(S), evaluate(synth_detect_table(S,T))],
    [],
    [table(S, T)]
).

action(
    get_field_types(T, FT),
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