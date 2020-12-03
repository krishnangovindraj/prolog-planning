:- module(simple_state, 
    [state_create/2, state_satisfies/2, state_apply_action/3, state_cleanup/2,
    state_update_loopdetector/4, state_check_loops/3]).

% Because it's hard otherwise
:- consult(state_query).

:- use_module(representation).

% In case we want to do loop detection
% Unfortunately, My state is so simple I haven't been keeping signatures. So all hashes(=0) collide
:- use_module(loop_detection).
:- use_module(state_hashing).
:- use_module(state_manipulation, [do_loop_detection/1]).

%% state_create(+PredicateList, -State).
state_create(PredicateList, PredicateList).

 % Produces every applicable action signature. Will be ground if State is ground.
 %state_satisfies(+Preconditions, +State).
 state_satisfies(Preconditions, State):-
    query_state(Preconditions, State).
    %query_in_list(Preconditions, State).

check_predicate_in_state(Predicate, State):-
    member(Predicate, State).

% Applies action on state 
% state_apply_action(+State, +Action, -ResultState)
state_apply_action(State, Action, ResultState):-
    action(Action, _Precond, Delete, Add),
    subtract(State, Delete, TempState),
    append(Add, TempState, ResultState).


% Doesn't do loop detection.
% state_update_loopdetector(+State, +ActionPath, +CurrentLoopDetector, -UpdatedLoopDetector). 
state_update_loopdetector(_, _, LD,[0|LD]).

% state_check_loops(+State, +LoopDetector, +ActionPath).
state_check_loops(_State, LoopDetector, ActionPath):- 
    do_loop_detection(true),
    loop_check(false, 0, LoopDetector, ActionPath). 


state_cleanup(_State, _ActionPath).