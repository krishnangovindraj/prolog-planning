:- module(representation, [
    action/4, initial_state/1, goal_check/2, constraint/1,
    action_signature/1, expand/3, is_applicable_action/2, violates_constraints/2]).

:- use_module(state_manipulation, [state_satisfies/2]).

:- dynamic action/4, initial_state/1, goal_check/2, constraint/1.

% initialize_problem(+ProblemFile).
initialize_problem(ProblemFile):-
    consult(ProblemFile).

% action_signature(?ActionSig).
action_signature(AS):-
    action(AS, _, _, _).

% expand(+ActionPath, +State, -Children)
expand(_ActionPath, State, Children):-
    % Find potential actions right?
    findall(ActionSig, (action_signature(ActionSig), is_applicable_action(ActionSig, State)), Children).


% Produces every applicable action signature. Will be ground if State is ground.
% is_applicable_action(+ActionSig, ?State)
is_applicable_action(ActionSig, State):-
    action(ActionSig, Preconditions, _, _), 
    state_satisfies(Preconditions, State).

% violates_constraints(+State, +ActionPath)
violates_constraints(State, _ActionPath):-
    constraint(C), not(state_satisfies(C, State)).
