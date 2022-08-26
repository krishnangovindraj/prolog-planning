% A naive but tidy generic DFS with loop detection from the state representations. 
:- module(forward_dfs, [search_forward_dfs/4]).
:- use_module(state_manipulation).

:- use_module(operations).

% search_forward_dfs(+StartState, +InterestPredicate, +MaxDepth, -Goals):-
search_forward_dfs(StartState, InterestPredicate, MaxDepth, Goals):-
    forward_dfs([], StartState, InterestPredicate, Goals, MaxDepth, []).

% ActionPath does not includes Action.
explore_branch(ActionPath, State, InterestPredicate, Goal, MaxDepth, LoopDetector):-
    not(state_check_loops(State, LoopDetector, ActionPath)),
    not(violates_constraints(State, ActionPath)),
    (
        (call(InterestPredicate, State, ActionPath), Goal = ActionPath);
        forward_dfs(ActionPath, State, InterestPredicate, Goal, MaxDepth,  LoopDetector)
    ).

forward_dfs_body(ActionPath, State, NextLevel, InterestPredicate, MaxDepth1, NextLoopDetector, NewGoals):-
    member(NextAction, NextLevel),
    state_apply_action(State, NextAction, NextState), % In case it fails
    (
        explore_branch([NextAction|ActionPath], NextState, InterestPredicate, NewGoals, MaxDepth1, NextLoopDetector);
        not(state_cleanup(NextState, [NextAction|ActionPath])) % Runs regardless of explore_branch success/failure.
    ).

forward_dfs(ActionPath, State, InterestPredicate, Goal, MaxDepth, LoopDetector):-
     % This is for the cut to not change behaviour. It can be commented out.
    MaxDepth > 0,  
    MaxDepth1 is MaxDepth - 1,
    expand(ActionPath, State, NextLevel),
    state_update_loopdetector(State, ActionPath, LoopDetector, NextLoopDetector),
    forward_dfs_body(ActionPath, State, NextLevel, InterestPredicate, MaxDepth1, NextLoopDetector, Goal).
