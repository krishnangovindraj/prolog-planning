
% :- module(synth_rpc, []).
:- use_module('../json_client.pl').

:- use_module(synth_wrangling).

% multi- A set of outputs (one-by-one)
% non-det: Multiple disjoint outputs 
% multi-non-det: multiple-disjoint sets of outputs

% TODO: Result caching layer to not make the same call over and over.
% TODO: Better argument names


%   %   %   %   %
%   Remote calls 
%   %   %   %   %
% det: +, -
synth_load_spreadsheet(Filename, SpreadsheetId):-
    query_synth(load_spreadsheet(Filename, SpreadsheetId)).

% +, -: multi-nondet.
synth_detect_tables(SpreadsheetId,TableList):-
    query_synth(detect_tables(SpreadsheetId, TableList)).

% +, - % det for now. No alternatives to the field_types.
synth_get_field_types(TableId, FieldTypes):-
    query_synth(get_field_types(TableId,FieldTypes)).

% +, - (multi-nondet or just non-det?)
synth_get_table_structure(TableId, TableStructurePreds):-
    query_synth(get_table_structure(TableId, TableStructurePreds)).

% +, - % Is det if countor learning is det :|
synth_learn_countor(tensor(TableId, AxisLabels, IndexMap), Constraints):-
    query_synth( learn_countor(TableId, AxisLabels, IndexMap, Constraints) ).



%   %   %   %   %
%   Local calls 
%   %   %   %   %
% +, - : non-det. 
synth_contains_tensor(TableId, tensor(TableId, AxisLabels, IndexMap), State):-
    synth_detect_tensors_impl(TableId, tensor(TableId, AxisLabels, IndexMap), State).