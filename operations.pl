:- module(operations, [
    initialize_problem/1,
    expand/3, is_applicable_action/2, violates_constraints/2,
    evaluate_predicate/1]).

:- use_module(planning_utils, [skolemize/1]).
:- use_module(representation).
:- use_module(state_manipulation, [state_satisfies/2, state_apply_action/3]).

:- dynamic initialize_problem__done/1.

% initialize_problem(+ProblemFile).
initialize_problem(ProblemFile):-
    initialize_problem__done(P),!,
    (
        (P = ProblemFile) -> 
        (writeln("WARN: Already initialized")); 
        (writeln(["ERROR: Session already initialized with ", ProblemFile]), fail)
    ).

initialize_problem(ProblemFile):- 
    representation:load_problem(ProblemFile),
    assert(initialize_problem__done(ProblemFile)).


% expand(+ActionPath, +State, -Children)
expand(_ActionPath, State, Children):-
    % TIL: The difference between findall and bagof
    (bagof(ActionSig, is_applicable_action(ActionSig, State), Children) -> true; Children=[]),
    planning_utils:skolemize(Children).


% Produces every applicable action signature. Will be ground if State is ground.
% is_applicable_action(+ActionSig, ?State)
is_applicable_action(ActionSig, State):-
    action(ActionSig, Preconditions, _, _, _), 
    state_satisfies(Preconditions, State).

% violates_constraints(+State, +ActionPath)
violates_constraints(State, _ActionPath):-
    constraint(C), not(state_satisfies(C, State)).

evaluate_predicate(Predicate):- 
    problem:evaluate_predicate(Predicate).

% +,-: non-det
evaluate_plan_sketch_with_final_state(PlanSketch, FinalState):-
    epswfs_do(PlanSketch, FinalState).

epswfs_do([],_):-
    !. % The cut is not needed really

epswfs_do([ActionSignature|RestOfActions], FinalState):-
    epswfs_do(RestOfActions, FinalState),
    action(ActionSignature, _PreCond, _DeleteList, _AddList, PerformList),
    perform_performlist(PerformList, FinalState). % This will ground ActionSignature (or fail).

% +,+,-: non-det
evaluate_plan_sketch(PlanSketch, StartState, FinalState):-
    eps_do(PlanSketch, StartState, FinalState).


eps_do([], State, State):-
    !. % The cut is not needed really

eps_do([ActionSignature|RestOfActions], StartState, ResultState ):-
    eps_do(RestOfActions, StartState, IntermediateState),
    action(ActionSignature, _PreCond, _DeleteList, _AddList, PerformList),
    perform_performlist(PerformList, IntermediateState),  % This will ground ActionSignature (or fail).
    state_apply_action(IntermediateState, ActionSignature, ResultState).


perform_performlist([], _).

perform_performlist([Perform|PerformList], State):-
    problem:perform_perform(Perform, State),
    perform_performlist(PerformList, State).
