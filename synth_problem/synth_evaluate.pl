:- module(synth_evaluate, []).
:- use_module('../json_client.pl').
:- use_module(state_manipulation).

:- consult('synth_sql.pl').
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
    % MLL == 0 .
    length(MList, MLL),
    MLL < 2. % < 2 if you're doing the final state version. else == 0.
    

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

% +, -, - % det for now. No alternatives to the field_types.
synth_get_field_types(TableId, FieldTypes, FieldTypeLength):-
    query_synth(get_field_types(TableId, FieldTypes)),
    length(FieldTypes, FieldTypeLength).

synth_get_incomplete_fields(TableId, NHeaderRows, IncompleteFieldList, IFLength):-
    query_synth(get_incomplete_fields(TableId, NHeaderRows, IncompleteFields)),
    findall(F/P , member(incomplete_field(F, P), IncompleteFields), IncompleteFieldList),
    length(IncompleteFieldList, IFLength).


% +, -, -, -
synth_get_field_headers(TableId, NHeaderRows, FieldHeaderList, FHLLength):-
    query_synth(get_field_headers(TableId, NHeaderRows, FieldHeaderList)),
    length(FieldHeaderList, FHLLength).

% % +, - (multi-nondet or just non-det?)
% synth_get_table_structure(TableId, TableStructurePreds):-
%     query_synth(get_table_structure(TableId, TableStructurePreds)).

% +, - % Is det if countor learning is det :|
synth_learn_countor(TensorId, Constraints):-
    query_synth( learn_countor(TensorId, Constraints) ).



%   %   %   %   %
%   Local calls 
%   %   %   %   %
% +, - : non-det. 
synth_contains_tensor(TableId, FieldHeaderList, tensor(TableId, AxisLabels, IndexMap)):-
    synth_detect_tensors_impl(TableId, FieldHeaderList, tensor(TableId, AxisLabels, IndexMap)).

synth_materialize_tensor(tensor(TableId, AxisLabels, IndexMap), TensorId):-
    query_synth(tensor_from_spec(TableId, AxisLabels, IndexMap, TensorId)).


synth_inner_join(
        T1Id, T2Id,      % +, +
        JoinSpec,      % in or out: ?
        State,   
        table(SSId, T3Id, NRows, NCols) % out: -, -, -, -
    ):-
    % Supports +- for 
    % % branch-point to backtrack over possibilities
    % check_predicate_in_state(field_header_list(T1Id, _, FHL1,_), State),
    % check_predicate_in_state(field_header_list(T2Id, _, FHL2,_), State),
    
    % member(JoinSpec, FHL1), member(JoinSpec, FHL2),  % This enables the ? on JoinField
    
    query_synth(join_tables(T1Id, T2Id, JoinSpec, SSId, T3Id, NRows, NCols)).



% Shady. This should ideally be a perform.
synth_join_candidate(FHL1, FHL2, JC):-
    (var(FHL1),!);
    (var(FHL2),!);
    (member(field_header(_, _, JC), FHL1), member(field_header(_, _, JC), FHL2)).

synth_projection_field_mapping(SrcHeaderList, _DstHeaderList, DstIncompleteList, FieldMapping, RemainingIncompleteFields):-
    findall(JC, synth_join_candidate( DstIncompleteList, SrcHeaderList, JC), AllJC),
    % TODO: Generate subsets.    
    % This looks wasteful because I haven't decided the format of DstIncompleteList yet, soz.
    findall(M/M , member(M, AllJC), FieldMapping),
    findall(M , (member(M, DstIncompleteList), not(member(M, AllJC))), RemainingIncompleteFields).
