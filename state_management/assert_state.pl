% We clean up states when we're done. That means we can't rely on them for loop detection.
:- module(assert_state, 
    [state_create/2, state_satisfies/2, state_apply_action/3, state_cleanup/2,
    state_update_loopdetector/4, state_check_loops/3]).

:- use_module(loop_detection).
:- use_module(state_hashing).

:- use_module(planning_utils).
:- use_module(representation).
:- use_module(state_manipulation, [do_loop_detection/1, hash_collision_is_loop/1]).


:- dynamic asserted_state/2.

reset:-
    retractall(asserted_state(_,_)),
    (nb_current(assert_state_counter, _), !; nb_setval(assert_state_counter, 0)).

counter_get_next(Y):-
    nb_getval(assert_state_counter, X),
    Y is X+1,
    nb_setval(assert_state_counter, Y).


%% create_state(+PredicateList, -State).
state_create(PredicateList, assert_state(StateId, Meta)):-
    reset,
    counter_get_next(StateId),
    update_sig(0, [], PredicateList, Sig),
    length(PredicateList, StateSize),
    unpack_meta(Meta, Sig, StateSize),
    foreach(member(X, PredicateList), assert(asserted_state(StateId, X))).

%state_satisfies(+Preconditions, +State).
state_satisfies(Preconditions, assert_state(StateId, _Meta)):-
    query_asserted_state(Preconditions, StateId).

query_asserted_state([], _StateId).
query_asserted_state([evaluate(P)|T], L):-
    !, % Cut so we don't try the member
    P,
    query_asserted_state(T, L).

query_asserted_state([not(P)|T], StateId):-
    !, % Cut so we don't try the member
    (is_list(P),!;  writeln('ERROR: L must be a list in not(L). FIX IT!'), fail),
    not(query_asserted_state(P, StateId)),
    query_asserted_state(T, StateId).

query_asserted_state([PreCond|T], StateId):-
    asserted_state(StateId, PreCond),
    query_asserted_state(T, StateId).


% Applies action on state 
% state_apply_action(+State, +Action, -ResultState)
state_apply_action(assert_state(StateId, Meta), Action, assert_state(ResultStateId, ResultMeta)):-
    action(Action, _Precond, DeleteList, AddList),
    counter_get_next(ResultStateId),
    foreach(
        asserted_state(StateId, S), 
        ( (member(S, DeleteList), !); assert(asserted_state(ResultStateId, S)) )
    ),
    foreach(
        member(S, AddList),
        assert(asserted_state(ResultStateId, S))
    ),
    update_meta(Meta, DeleteList, AddList, ResultMeta).

% state_update_loopdetector(+State, +ActionPath, +CurrentLoopDetector, -UpdatedLoopDetector). 
state_update_loopdetector(assert_state(_StateId, Meta), _ActionPath, LD, [Sig|LD]):-
    unpack_meta_signature(Meta, Sig).

% state_check_loops(+State, +LoopDetector, +ActionPath).
state_check_loops(assert_state(_StateId, Meta), LoopDetector, ActionPath):- 
    do_loop_detection(true),
    unpack_meta_signature(Meta, Sig),
    hash_collision_is_loop(CheckOnlyHashes),
    loop_check(CheckOnlyHashes, Sig, LoopDetector, ActionPath).

% Good habit so I see compile time if I get the signature wrong.
unpack_meta_signature(Meta, Sig):-
    unpack_meta(Meta, Sig, _StateSize).

unpack_meta(meta(Sig, StateSize), Sig, StateSize).


update_meta(Meta, DeleteList, AddList, UpdatedMeta):-
    unpack_meta(Meta, Sig, StateSize),
    
    length(DeleteList, DLL), length(AddList, ALL),
    UpdatedStateSize is StateSize + ALL - DLL,
    update_sig(Sig, DeleteList, AddList, UpdatedSig),
    unpack_meta(UpdatedMeta, UpdatedSig, UpdatedStateSize).

update_sig(InitialSig, DeleteList, AddList, ResultSig):-
    % The combination operator needs to be distributive, commutative, associative.
    update_hash(InitialSig, DeleteList, AddList, ResultSig).

state_cleanup(assert_state(StateId, _Meta), _ActionPath):-
    retractall(asserted_state(StateId,_)).
