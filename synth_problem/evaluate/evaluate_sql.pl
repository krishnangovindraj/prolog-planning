% +, - : non-det. 
synth_inner_join(
        T1Id, T2Id,      % +, +
        JoinSpec,      % in or out: ?
        _State,   
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
