:- module(assert_state, 
    [state_create/2, state_satisfies/2, state_apply_action/3, 
    state_update_loopdetector/4, state_check_loops/3]).
:- use_module(planning_utils).
:- use_module(representation).

:- use_module(state_hashing).

:- dynamic asserted_state/2.

% Some settings?
% If true, We don't compare states, just hashes. If false, We apply actions backwards and see if they cancel out.
do_loop_detection(true).
hash_collision_is_loop(false).


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
state_update_loopdetector(assert_state(StateId, Meta), _ActionPath, LD, [Sig/StateSize/StateId|LD]):-
    unpack_meta(Meta, Sig, StateSize).


% state_check_loops(+State, +LoopDetector, +ActionPath).
state_check_loops(assert_state(StateId, Meta), LoopDetector, _ActionPath):-  
    do_loop_detection(true),
    unpack_meta(Meta, Sig, StateSize),
    hash_collision_is_loop(CheckOnlyHashes),
    (CheckOnlyHashes -> 
        (member(Sig/StateSize/_, LoopDetector));
        (assert_state_loop_check(StateId, Sig, StateSize, LoopDetector, _LazyStateCache))
    ).

get_lazy_state_cache(StateId, LazyStateCache):-
    not(ground(LazyStateCache))-> findall(S, asserted_state(StateId,S), LazyStateCache).

% 
assert_state_loop_check(StateId, Sig, StateSize, [Sig/StateSize/OtherStateId|LoopDetector], LazyStateCache):-
    get_lazy_state_cache(StateId, LazyStateCache),
    (
        (query_asserted_state(LazyStateCache, OtherStateId),!); % This + state size matching is sufficient
        assert_state_loop_check(StateId, Sig, StateSize, LoopDetector, LazyStateCache)
    ).
    
assert_state_loop_check(StateId, Sig, StateSize, [_|LoopDetector], LazyStateCache):-
    assert_state_loop_check(StateId, Sig, StateSize, LoopDetector, LazyStateCache).

% Good habit so I see compile time if I get the signature wrong.
unpack_meta_signature(meta(Sig), Sig).
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
