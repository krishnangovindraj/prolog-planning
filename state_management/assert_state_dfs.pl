% TODO: Cleans up nodes, but loop checking relies on earlier states being in memory. Hence only works for DFS.
% For other search techniques, use the assert_state version that uses the action-replay loop checking.
:- module(assert_state_dfs, 
    [state_create/2, state_satisfies/2, state_apply_action/3, state_cleanup/2,
    state_update_loopdetector/4, state_check_loops/3]).


:- consult(state_query).

:- use_module(representation).

:- use_module(state_hashing).
:- use_module(state_manipulation, [do_loop_detection/1, hash_collision_is_loop/1]).

:- dynamic asserted_state_d/2.

reset:-
    retractall(asserted_state_d(_,_)).

init_counter:-
    (nb_current(assert_state_d_counter, _), !; nb_setval(assert_state_d_counter, 0)).


counter_get_next(Y):-
    nb_getval(assert_state_d_counter, X),
    Y is X+1,
    nb_setval(assert_state_d_counter, Y).


%% create_state(+PredicateList, -State).
state_create(PredicateList, assert_state_d(StateId, Meta)):-
    init_counter,
    counter_get_next(StateId),
    update_sig(0, [], PredicateList, Sig),
    length(PredicateList, StateSize),
    unpack_meta(Meta, Sig, StateSize),
    foreach(member(X, PredicateList), assert(asserted_state_d(StateId, X))).

%state_satisfies(+Preconditions, +State).
state_satisfies(Preconditions, assert_state_d(StateId, _Meta)):-
    query_state(Preconditions, StateId).

check_predicate_in_state(Predicate, StateId):-
    asserted_state_d(StateId, Predicate).

% Applies action on state 
% state_apply_action(+State, +Action, -ResultState)
state_apply_action(assert_state_d(StateId, Meta), Action, assert_state_d(ResultStateId, ResultMeta)):-
    action(Action, _Precond, DeleteList, AddList),
    counter_get_next(ResultStateId),
    foreach(
        asserted_state_d(StateId, S), 
        ( (member(S, DeleteList), !); assert(asserted_state_d(ResultStateId, S)) )
    ),
    foreach(
        member(S, AddList),
        assert(asserted_state_d(ResultStateId, S))
    ),
    update_meta(Meta, DeleteList, AddList, ResultMeta).

% state_update_loopdetector(+State, +ActionPath, +CurrentLoopDetector, -UpdatedLoopDetector). 
state_update_loopdetector(assert_state_d(StateId, Meta), _ActionPath, LD, [Sig/StateSize/StateId|LD]):-
    unpack_meta(Meta, Sig, StateSize).


% state_check_loops(+State, +LoopDetector, +ActionPath).
state_check_loops(assert_state_d(StateId, Meta), LoopDetector, _ActionPath):-  
    do_loop_detection(true),
    unpack_meta(Meta, Sig, StateSize),
    hash_collision_is_loop(CheckOnlyHashes),
    (CheckOnlyHashes -> 
        (member(Sig/StateSize/_, LoopDetector));
        (assert_state_d_loop_check(StateId, Sig, StateSize, LoopDetector, _LazyStateCache))
    ).

get_lazy_state_cache(StateId, LazyStateCache):-
    not(ground(LazyStateCache))-> findall(S, asserted_state_d(StateId,S), LazyStateCache).

% 
assert_state_d_loop_check(StateId, Sig, StateSize, [Sig/StateSize/OtherStateId|LoopDetector], LazyStateCache):-
    get_lazy_state_cache(StateId, LazyStateCache),
    (
        (query_state(LazyStateCache, OtherStateId),!); % This + state size matching is sufficient
        assert_state_d_loop_check(StateId, Sig, StateSize, LoopDetector, LazyStateCache)
    ).
    
assert_state_d_loop_check(StateId, Sig, StateSize, [_|LoopDetector], LazyStateCache):-
    assert_state_d_loop_check(StateId, Sig, StateSize, LoopDetector, LazyStateCache).

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

state_cleanup(assert_state_d(StateId, _Meta), _ActionPath):-
    retractall(asserted_state_d(StateId, _)).
