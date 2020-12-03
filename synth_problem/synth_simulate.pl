
% :- module(synth_rpc, []).
:- use_module('../json_client.pl').

% TODO: Result caching layer to not make the same call over and over.
% TODO: Better argument names

% TODO: Think about regression planning. I can probably leave A LOT of this out if i go backwards.


:- dynamic 
    simulated_spreadsheet_id/2,
    simulated_detect_tables/2.



simulate_predicate(synth_load_spreadsheet(Filename, SpreadsheetId), State):-
    (simulated_spreadsheet_id(Filename, SpreadsheetId),!);
    (gen_sym('ss_sim_', SpreadsheetId), assert(simulated_spreadsheet_id(Filename, SpreadsheetId))).
    

% nondet: +, -
simulate_predicate(synth_detect_tables(SpreadsheetId,TableId), State):-
    TableList = [T1,T2],
    (
        (simulated_detect_tables(SpreadsheetId, TableList), !);
        gen_sym('tbl_sim_', T1), gen_sym('tbl_sim_', T2), assert(simulated_detect_tables(SpreadsheetId,TableList))
    ),
    member(TableId, TableList).

% nondet: +, -
simulate_predicate(synth_get_field_types(TableId, FieldTypes), State):-
    % Leave an unbound type. simulate 3 fields
    between(0,3, FI), FieldTypes = field_type(TableId, FI, _).

% nondet: +, -
% This is icky and I'm not proud of it.
simulate_predicate(synth_get_table_structure(TableId, TableStructurePreds) , State):-
    writeln("Called simulate synth_get_table_structure"),
    member(TableStructurePreds, [
        table_n_fields(TableId, NF),
        table_data_range(TableId, DS, X), % Yuck yuck. Go backwards def.
        table_header_row(TableId, TH),
        table_field_title(TableId, FTI, [F0H1, F0H2]) %
    ]),
    evaluate(synth_get_table_structure(TableId, TableStructurePreds)).
    
% +, -
simulate_predicate(synth_learn_countor(tensor(TableId, AxisLabels, IndexMap), Constraints), State):-
    fail.
    


%   %   %   %   %
%   Local calls 
%   %   %   %   %
% +, -
simulate_predicate(synth_contains_tensor(TableId, tensor(TableId, AxisLabels, IndexMap), State):-
    
    fail.