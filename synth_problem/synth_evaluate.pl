
% :- module(synth_rpc, []).
:- use_module('../json_client.pl').

:- use_module(synth_wrangling).

:- dynamic
    data_source/2,
    data_source_path/2. % 

% TODO: Result caching layer to not make the same call over and over.
% TODO: Better argument names


%   %   %   %   %
%   Remote calls 
%   %   %   %   %
% det: +, -
synth_load_spreadsheet(Filename, SpreadsheetId):-
    query_synth(load_spreadsheet(Filename, SpreadsheetId)).

% nondet: +, -
synth_detect_tables(SpreadsheetId,TableId):-
    query_synth(detect_tables(SpreadsheetId, TableId)).

% nondet: +, -
synth_get_field_types(TableId, FieldTypes):-
    query_synth(get_field_types(TableId,FieldTypes)).

% nondet: +, -
synth_get_table_structure(TableId, TableStructurePreds):-
    query_synth(get_table_structure(TableId, TableStructurePreds)).

% +, -
synth_learn_countor(tensor(TableId, AxisLabels, IndexMap), Constraints):-
    query_synth( learn_countor(TableId, AxisLabels, IndexMap, Constraints) ).



%   %   %   %   %
%   Local calls 
%   %   %   %   %

synth_contains_tensor(TableId, tensor(TableId, AxisLabels, IndexMap), State):-
    synth_detect_tensors_impl(TableId, tensor(TableId, AxisLabels, IndexMap), State).