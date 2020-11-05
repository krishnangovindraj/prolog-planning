:- module(state_manipulation, [state_create/2, state_satisfies/2, state_apply_action/3, 
    state_update_loopdetector/4, state_check_loops/3]).

:- use_module('state_management/simple_state.pl').
% :- use_module('state_management/ordered_state.pl').
% :- use_module('state_management/assert_state.pl').
