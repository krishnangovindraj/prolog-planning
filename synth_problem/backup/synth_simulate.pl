:- use_module(state_manipulation).

simulate(synth_load_spreadsheet(_Filename, SpreadsheetId), _State):-
   gensym('sim__ss_', SpreadsheetId). 

% +, -: multi-nondet.
cannot_simulate(synth_detect_tables(_SpreadsheetId, _TableList, _TLLength)).

% +, - % det for now. No alternatives to the field_types.
cannot_simulate(synth_get_field_types(TableId, FieldTypes, FieldTypeLength)).

cannot_simulate(synth_get_field_headers(TableId, NHeaderRows, FieldHeaderList, FHLLength)).
% % +, - (multi-nondet or just non-det?)
% synth_get_table_structure(TableId, TableStructurePreds):-
%     query_synth(get_table_structure(TableId, TableStructurePreds)).

% +, - % Is det if countor learning is det :|
simulate(synth_learn_countor(tensor(TableId, AxisLabels, IndexMap), countor_constraint_list(Constraints, NConstraints)), State):-
    % Pretend we have 3 constraints
    findall(C, (between(1,3, _), (sim__countor_, C)), Constraints).
