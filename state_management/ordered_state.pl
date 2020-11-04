%% Uses ordered sets (from the ordsets library) instead of regular lists.
%% Ordered sets are actually just lists, so insertion and deletion is still slow.
:- module(ordered_state, 
    [state_create/2, state_satisfies/2, state_apply_action/3, 
    state_update_loopdetector/4, state_check_loops/3]).

:- use_module(loop_detection).
:- use_module(state_hashing).

:- use_module(planning_utils).
:- use_module(representation).
:- use_module(library(ordsets)).

% Some settings?
% If true, We don't compare states, just hashes. If false, We apply actions backwards and see if they cancel out.
hash_collision_is_loop(true).


%% create_state(+PredicateList, -State).
state_create(PredicateList, ordered_state(OrderedSet, Meta)):-
    list_to_ord_set(PredicateList, OrderedSet),
    update_sig(0, [], PredicateList, Sig),
    unpack_meta(Meta, Sig).

%state_satisfies(+Preconditions, +State).
state_satisfies(Preconditions, ordered_state(State, _Meta)):-
    % We cannot take much advantage of the sorted-ness because it does not produce all assignments.
    query_in_list(Preconditions, State).


% Applies action on state 
% state_apply_action(+State, +Action, -ResultState)
state_apply_action(ordered_state(State, Meta), Action, ordered_state(ResultState, ResultMeta)):-
    action(Action, _Precond, DeleteList, AddList),
    list_to_ord_set(DeleteList, Delete),
    list_to_ord_set(AddList, Add),
    ord_subtract(State, Delete, TempState),
    update_meta(Meta, DeleteList, AddList, ResultMeta),
    ord_union(Add, TempState, ResultState).

% state_update_loopdetector(+State, +ActionPath, +CurrentLoopDetector, -UpdatedLoopDetector). 
state_update_loopdetector(ordered_state(_State, Meta), _ActionPath, LD, [Sig|LD]):-
    unpack_meta_signature(Meta, Sig).

state_check_loops(ordered_state(_, Meta), LoopDetector, ActionPath):- 
    unpack_meta_signature(Meta, Sig),
    hash_collision_is_loop(CheckOnlyHashes),
    loop_check(CheckOnlyHashes, Sig, LoopDetector, ActionPath).


% Good habit so I see compile time if I get the signature wrong.
unpack_meta_signature(meta(Sig), Sig).
unpack_meta(meta(Sig), Sig).


update_meta(Meta, DeleteList, AddList, UpdatedMeta):-
    unpack_meta(Meta, Sig),
    update_sig(Sig, DeleteList, AddList, UpdatedSig),
    unpack_meta(UpdatedMeta, UpdatedSig).

update_sig(InitialSig, DeleteList, AddList, ResultSig):-
    % The combination operator needs to be distributive, commutative, associative.
    update_hash(InitialSig, DeleteList, AddList, ResultSig).

update_sig(InitialSig, DeleteList, AddList, ResultSig):-
    MOD is 1000000007,
    update_sig_delete(InitialSig, DeleteList, MOD, TempSig),
    update_sig_add(TempSig, AddList, MOD, ResultSig).
    

update_sig_delete(Sig, [], _, Sig).
update_sig_delete(InSig, [D|DT], MOD, OutSig):-
    term_hash(D, Piece),
    TempSig is (InSig + MOD - Piece) mod MOD,  
    update_sig_delete(TempSig, DT, MOD, OutSig).

update_sig_add(Sig, [], _, Sig).
update_sig_add(InSig, [A|AT], MOD, OutSig):-
    term_hash(A, Piece),
    TempSig is (InSig + Piece) mod MOD,  
    update_sig_add(TempSig, AT, MOD, OutSig).

