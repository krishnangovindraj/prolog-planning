:- module(representation, [
    action/4, initial_state/1, goal_check/2,
    action_signature/1, expand/3, is_applicable_action/2]).

:- use_module(state_manipulation, [state_satisfies/2]).

:- dynamic action/4, initial_state/1, goal_check/2.

initialize_problem(X):-
    consult(X).

action_signature(AS):-
    action(AS, _, _, _).


expand(_ActionPath, State, Children):-
    % Find potential actions right?
    findall(ActionSig, (action_signature(ActionSig), is_applicable_action(ActionSig, State)), Children).


 % Produces every applicable action signature. Will be ground if State is ground.
 is_applicable_action(ActionSig, State):-
    action(ActionSig, Preconditions, _, _), 
    state_satisfies(Preconditions, State).