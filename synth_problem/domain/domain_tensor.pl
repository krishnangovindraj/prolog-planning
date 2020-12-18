% Section 2: Data wrangling?
% TODO: Move datatype & shape determination to contains_tensor
action(
    detect_tensors(TableId, FieldHeaderList, TsrId, TsrSpec, tensor_meta(Shape, DataType, Density)),
    [table(_S, TableId, _NRows, _NCols), field_header_list(TableId, _NHeaderRows, FieldHeaderList, _),
    evaluate(synth_safe_evaluate([TableId, FieldHeaderList], synth_contains_tensor(TableId, FieldHeaderList, TsrSpec)))],
    [],
    [ tensor(TableId, TsrId, tensor_meta(Shape, DataType, Density)) ], % TODO and the tensor is a compound thingy. This data-type isn't properly determined.
    [
        % perform(synth_contains_tensor(TableId, FieldHeaderList, TsrSpec), _), 
        % perform(no_duplicate_in_state(tensor_spec(_, TsrSpec, DataType), State), State), % Better: replace this with some hash.
        perform(synth_materialize_tensor(TsrSpec, TsrId, tensor_meta(Shape, DataType, Density))) % Really need virtual views at this point.
    ]
).

action(
    fold_tensor(TableId, TsrId, DataType, AxisI, AggOp, NewTsrId, NewShape, NewDensity),
    [tensor(TableId, TsrId, tensor_meta(OriginalShape, DataType, Density)),
        evaluate(synth_safe_evaluate([Density], Density >= 1)),
        evaluate(synth_safe_evaluate([OriginalShape], synth_tensor_fold_meta(OriginalShape, AxisI, NewShape)))
        evaluate(synth_aggregate_operator_choice(DataType, AggOp))],
    [],
    [tensor(TableId, NewTsrId, tensor_meta(NewShape, DataType, Density))],
    [perform(synth_fold_tensor(TsrId, AxisI, AggOp, tensor(TableId, NewTsrId, tensor_meta(NewShape, DataType, NewDensity))))]
).

action(
    learn_countor(TsrId, Constraints),
    [tensor(_TableId, TsrId, tensor_meta(_, _, Density)), not([countor_constraint(TsrId, Constraints)]),
     evaluate(synth_safe_evaluate([Density], Density >= 1 ))], % learn only from full?
    [],
    [countor_constraint(TsrId, Constraints)],
    [perform(synth_learn_countor(TsrId, Constraints))]
).

action(
    fill_countor(IncompleteTsrId, Constraints, NewTsrId),
    [countor_constraint(CTsrId, Constraints),
        tensor(_TableId, IncompleteTsrId, tensor_meta(Shape, _, Density)), 
        not([filled_countor(IncompleteTsrId, Constraints, NewTsrId)]),
        tensor(_, CTsrId, tensor_meta(Shape, _, _)), % The shapes must match.
        evaluate(synth_safe_evaluate([Density], Density < 1 ))],
    [],
    [filled_countor(IncompleteTsrId, Constraints, NewTsrId)],
    [perform(synth_fill_countor(Constraints, IncompleteTsrId, NewTsrId))]
).
