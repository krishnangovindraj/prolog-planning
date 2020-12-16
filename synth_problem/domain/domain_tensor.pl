% Section 2: Data wrangling?

action(
    detect_tensors(TableId, FieldHeaderList, Tsr, DataType),
    [table(_S, TableId, _NRows, _NCols), field_header_list(TableId, FieldHeaderList, _)],
    [],
    [ tensor(TsrId, DataType) ], % TODO and the tensor is a compound thingy. This data-type isn't properly determined.
    [
        perform(synth_detect_tensors(TableId, FieldHeaderList, TsrSpec, DataType), _), 
        perform(no_duplicate_in_state(tensor_spec(_, TsrSpec, DataType), State), State), % Better: replace this with some hash.
        perform(synth_register_tensor(TsrSpec, TsrId)) % Really need virtual views at this point.
    ]
).

action(
    fold_tensor(TableId, Tsr, AxisI, AggOp, NewTsr, DataType),
    [tensor(TsrId, DataType), evaluate(aggregate_operator(DataType, AggOp))],
    [],
    [tensor(NewTsrId, DataType)],
    [perform(synth_fold_tensor(TsrId, AxisI, AggOp, NewTsrId))]
).
