:- module(state_manipulation, [
    state_create/2, state_satisfies/2, state_apply_action/3, 
    state_update_loopdetector/4, state_check_loops/3,
    do_loop_detection/1, hash_collision_is_loop/1]).

:- use_module('state_management/simple_state.pl').
% :- use_module('state_management/ordered_state.pl').
% :- use_module('state_management/assert_state.pl').



% Some settings
% Whether we do loop detection at all.
do_loop_detection(true).

% If true, We don't compare states, just hashes. If false, We do compare states for equality.
% ordered_state: apply actions backwards and see if they cancel out.
% assert_state: We compare the states.
% Always false for simple_state.
hash_collision_is_loop(false).
