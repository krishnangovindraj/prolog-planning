:- module(por_commutativity, [cache_disabled_set/0, query_disabler_actions/2]).
% Create an index which let's us determine what to wake-up.
:- use_module(problem). % [action/5]

:- dynamic cache_disabled_set_complete/0, cached_disabled_action_list/2, cached_disabler_action/2.

cache_disabled_set:- cache_disabled_set_complete, !.

cache_disabled_set:-
    action(ActionSig, _, _, _, _),
    compute_disabled_action_set(ActionSig, DisabledActionList),
    % writeln([ActionSig, DisabledActionList]),
    do_cache_disabled_set(ActionSig, DisabledActionList),
    fail.

cache_disabled_set:- assert(cache_disabled_set_complete).

do_cache_disabled_set(ActionSig, DisabledActionList):-
    assert(cached_disabled_action_list(ActionSig, DisabledActionList)),
    foreach(member(DA, DisabledActionList), 
        assert( cached_disabler_action(DA, ActionSig)) ).

compute_disabled_action_set(PerformedAction, DisabledActionList):-
    action(PerformedAction, _, DeleteList,_, _),
    bagof(DisabledAction, 
        (find_disabled_action(DeleteList, DisabledAction), DisabledAction \== PerformedAction),
        DisabledActionList), !.

compute_disabled_action_set(_, []).

find_disabled_action(DeleteList, DisabledAction):-
        action(DisabledAction, Precond, _, _, _),
        member(P, DeleteList), member(P, Precond).

query_disabler_actions(DisabledAction, DisablerAction):-
    cached_disabler_action(DisabledAction, DisablerAction).
