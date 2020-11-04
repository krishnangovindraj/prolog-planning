:- use_module(representation).
:- use_module(state_manipulation).
:- use_module(forward_prop).

% Usage: 
%   ?- representation:initialize_problem(blocks_plan). % Once
%   ?- initial_state(SL), state_create(SL, S),!, search_forward_dfs(S, goal_check, 4, G). % As many as you want.
%   
% You can use a different MaxDepth. If you have loop detection on, you can set MaxDepth to -1 (no depth limit).
