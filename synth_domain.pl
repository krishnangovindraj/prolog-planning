:- consult(synth_rpc).
% From the pddl: 
%%  (:types table sheet model - object
%%          countor-constraint tacle-constraint segmentation dreaml mercs admercs lgg-rule - model)
action(
    load_spreadsheet(S),
    [data_source(S, spreadsheet), evaluate(synth_load_spreadsheet(S))], % I can add something 
    [],
    [spreadsheet(S)]
    ).

action(
    detect_tables(S,T), 
    [spreadsheet(S), evaluate(synth_detect_table(S,T))],
    [],
    [table(S, T)]
).
