:- module(synth_evaluate, []).
:- use_module('../json_client.pl').
:- use_module(state_manipulation).

:- use_module(synth_wrangling).

% multi- A set of outputs (one-by-one)
% non-det: Multiple disjoint outputs 
% multi-non-det: multiple-disjoint sets of outputs

% TODO: Result caching layer to not make the same call over and over.
% TODO: Better argument names

% Utils: Maybe I should move this to planning_utils?
% Checks if there are 2 term which match exactly with Member. (i.e., one other than member itself) 
% -, +, +
no_duplicate_in_state(Member, State):-
     % this:member/2 as  ==:=.
    copy_term(Member, MemberTemplate),
    findall( M, (
            M = MemberTemplate,
            check_predicate_in_state(M, State), % unifies
            M == Member % Checks if unification was required to make them equal.
        ), MList),
    length(MList, MLL),
    MLL < 2.


%   %   %   %   %
%   Remote calls 
%   %   %   %   %
% det: +, -
synth_load_spreadsheet(Filename, SpreadsheetId):-
    query_synth(load_spreadsheet(Filename, SpreadsheetId)).

% +, -: multi-nondet.
synth_detect_tables(SpreadsheetId, TableList, TLLength):-
    query_synth(detect_tables(SpreadsheetId, TableList)),
    length(TableList, TLLength).

% +, - % det for now. No alternatives to the field_types.
synth_get_field_types(TableId, FieldTypes, FieldTypeLength):-
    query_synth(get_field_types(TableId, FieldTypes)),
    length(FieldTypes, FieldTypeLength).

synth_get_field_headers(TableId, NHeaderRows, FieldHeaderList, FHLLength):-
    query_synth(get_field_headers(TableId, NHeaderRows, FieldHeaderList)),
    length(FieldHeaderList, FHLLength).

% % +, - (multi-nondet or just non-det?)
% synth_get_table_structure(TableId, TableStructurePreds):-
%     query_synth(get_table_structure(TableId, TableStructurePreds)).

% +, - % Is det if countor learning is det :|
synth_learn_countor(tensor(TableId, AxisLabels, IndexMap), Constraints):-
    query_synth( learn_countor(TableId, AxisLabels, IndexMap, Constraints) ).



%   %   %   %   %
%   Local calls 
%   %   %   %   %
% +, - : non-det. 
synth_contains_tensor(TableId, FieldHeaderList, tensor(TableId, AxisLabels, IndexMap)):-
    synth_detect_tensors_impl(TableId, FieldHeaderList, tensor(TableId, AxisLabels, IndexMap)).
