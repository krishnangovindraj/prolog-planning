
% Changed for brevity, uses state assuming unique field_{header,type}_list given TableId
% action(
%     inner_join_tables(T1Id, T2Id, FHL1, FHL2, JoinFieldList, T3Id),
%     [table(_S1, T1Id, _, _), table(_S2, T2Id, _, _), field_header_list(T1Id, _, _, _), field_header_list(T2Id, _, _, _) ],
%     [],
%     [table(joined(T1Id, T2Id), T3Id, T3Rows, T3Cols), field_header_list(T3Id, T3HeaderRows, FHL3, FHLL3)],
%     [
%         perform(synth_generate_join_candidates(FHL1, FHL2, JoinFieldList), _), 
%         perform(synth_inner_join(T1Id, T2Id, FHL1, FHL2, JoinFieldList, T3Id, T3HeaderRows, FHL3, FHLL3), _)
%     ]
% ).
% 
action(
    inner_join_tables(T1Id, T2Id, JoinField, T3Id),
    [table(_S1, T1Id, _, _, _), table(_S2, T2Id, _, _), 
        field_header_list(T1Id, _NHR1, FHL1, _), field_header_list(T2Id, _NHR2, FHL2, _ ), % Require same NHR?
        evaluate( synth_join_candidate(FHL1, FHL2, JoinField) ) ], % Will succeed with variable if FHL1 or FHL2 is variable
    [],
    [table(joined(T1Id, T2Id), T3Id, T3Rows, T3Cols)], % TODO , field_header_list(T3Id, T3HeaderRows, FHL3, FHLL3), field_type_list()],
    [
        perform(synth_inner_join(T1Id, T2Id, JoinField, State, table(join(T1Id, T2Id), T3Id, T3Rows, T3Cols)), State)
    ]
).

% % The SQL `SELECT` clause
% action(
%     fill_by_projection(SrcTId, DstTId, FieldMapping, DstIncompleteList, RemainingIncompleteFields),
%     [table(_S1, SrcTId, _NR1, _NC1), table(_S2, DstTId, _NR2, _NC2), 
%         % Add some incompleteness check.
%         field_header_list(SrcTId, SrcHeaderList, SHLL), field_header_list(DstTId, DstHeaderList, DHLL),
%         incomplete_field_list(DstTId, DstIncompleteList, DILL),
%     evaluate(projection_field_mapping(SrcHeaderList, DstHeaderList, DstIncompleteList, FieldMapping, RemainingIncompleteFields))
%         % Not yet. For now we do just headers:
%         %   field_type_list(SrcTId, SrcTypeList, STLL), field_type_list(DstTId, DstTypeList, DTLL), % These could be empty and may not have types.
%         % evaluate(projection_field_mapping(SrcHeaderList, SrcTypeList, DstHeaderList, DstTypeList, DstIncompleteList, FieldMapping, RemainingIncompleteFields))
%     ],
%     [incomplete_field_list(DstTId, DstIncompleteList, DILL)],
%     [incomplete_field_list(DstTId, RemainingIncompleteFields, DILL)],
%     []
% ).

% It's time to think about virtual views. 
% Where we have a base data-set (+models) and all derived tables are represented by operations on the base-datasets.