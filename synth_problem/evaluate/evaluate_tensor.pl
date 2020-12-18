:- use_module(evaluate_wrangling).

% +, - % Is det if countor learning is det :|
synth_contains_tensor(TableId, FieldHeaderList, tensor_spec(TableId, AxisLabels, IndexMap)):-
    evaluate_wrangling:synth_detect_tensors_impl(TableId, FieldHeaderList, AxisLabels, IndexMap).

synth_materialize_tensor(tensor_spec(TableId, AxisLabels, IndexMap), TensorId, TensorMeta):-
    query_synth(tensor_from_spec(TableId, AxisLabels, IndexMap, TensorId, TensorMeta)).

synth_fold_tensor(TsrId, AxisI, AggOp, NewTsrId):-
    query_synth(fold_tensor(TsrId, AxisI, AggOp, NewTsrId)).

synth_learn_countor(TensorId, Constraints):-
    query_synth( learn_countor(TensorId, Constraints) ).

synth_generate_countor(Constraints, NewTensorId):-
    query_synth( generate_countor(Constraints, NewTensorId) ).

synth_fill_countor(Constraints, FillTensorId, NewTensorId):-
    query_synth( fill_countor(Constraints, FillTensorId, NewTensorId) ).


synth_tensor_fold_meta(OriginalShape, FoldAxis, NewShape):-
    evaluate_wrangling:synth_tensor_fold_meta_impl(OriginalShape, FoldAxis, NewShape).


% TODO: 
% synth_aggregate_operator_choice(dt_real, op_max).
% synth_aggregate_operator_choice(dt_real, op_min).
% synth_aggregate_operator_choice(dt_real, op_sum).
% synth_aggregate_operator_choice(dt_real, op_avg).