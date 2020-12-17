:- use_module(representation).
:- use_module(operations).
:- use_module(state_manipulation).
:- use_module(forward_dfs).
:- use_module(interactive).

% Usage: 
%   ?- operations:initialize_problem('examples/blocks_plan.pl'). % Once
%   ?- initial_state(SL), state_create(SL, S),!, search_forward_dfs(S, goal_check, 4, G). % As many as you want.
%   
% You can use a different MaxDepth. If you have loop detection on, you can set MaxDepth to -1 (no depth limit).

:- consult('kg_hax.pl').
