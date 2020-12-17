:- module(operations, [
    initialize_problem/1,
    expand/3, is_applicable_action/2, violates_constraints/2,
    evaluate_predicate/2, simulate_predicate/2]).

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


% _State is for future use with simulate.
evaluate_predicate(Predicate, _State):- 
    Predicate. % call

% You can simulate certain actions so the planner can hyptothesize about likely outcomes and look more steps ahead.
% Be careful when using unbound variables. They're powerful but quirky.
% If you want a predicate to be evaluated instead of simulated, just don't declare a simulation
% If you don't want that to happen and want the user to explicitly choose to perform this action (and dependencies), 
% you can fail (or succeed by adding a must_evaluate(Action) and then specify must_evaluate(_) as part a potential  goal?)
simulate_predicate(_Predicate, _State):-
    writeln("Simulate doesnt' exist yet :("), fail.

% +,-: non-det
evaluate_plan_sketch_with_final_state(PlanSketch, FinalState):-
    deskolemize(PlanSketch, DPS),
    epswfs_do(DPS, FinalState).

epswfs_do([],_):-
    !. % The cut is not needed really

epswfs_do([ActionSignature|RestOfActions], FinalState):-
    epswfs_do(RestOfActions, FinalState),
    action(ActionSignature, _PreCond, _DeleteList, _AddList, PerformList),
    perform_performlist(PerformList, FinalState). % This will ground ActionSignature (or fail).

% +,+,-: non-det
evaluate_plan_sketch(PlanSketch, StartState, FinalState):-
    planning_utils:deskolemize(PlanSketch, DPS),
    eps_do(DPS, StartState, FinalState).


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
