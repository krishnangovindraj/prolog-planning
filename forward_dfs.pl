% A naive but tidy generic DFS with loop detection from the state representations. 
:- module(forward_dfs, [search_forward_dfs/4]).
:- use_module(state_manipulation).

:- use_module(representation).

% search_forward_dfs(+StartState, +InterestPredicate, +MaxDepth, -Goals):-
search_forward_dfs(StartState, InterestPredicate, MaxDepth, Goals):-
    forward_dfs([], StartState, InterestPredicate, Goals, MaxDepth, []).

%% forward_dfs_level(+NextLevelActions, +ActionPathTillHere, +State, +InterestPredicate, +GoalAcc, -GoalsReached, +MaxDepth, +LoopDetector).
forward_dfs_level([], _ActionPath, _State, _InterestPredicate, GoalAcc, GoalAcc, _MaxDepth, _LoopDetector).
forward_dfs_level([NextAction|SiblingActions], ActionPath, State, InterestPredicate, GoalAccIn, GoalAccOut, MaxDepth, LoopDetector):-
    % Recurse - 
    (state_apply_action(State, NextAction, ResultState) ->  % In case it fails
        forward_dfs([NextAction|ActionPath], ResultState, InterestPredicate, BranchGoals, MaxDepth,  LoopDetector); 
        BranchGoals = []
    ),
    % Process remaining children
    append(BranchGoals, GoalAccIn, GoalAccTemp),
    forward_dfs_level(SiblingActions, ActionPath, State, InterestPredicate, GoalAccTemp, GoalAccOut, MaxDepth, LoopDetector).

% forward_dfs(+ActionPath, +State, +InterestPredicate, -Goals, +MaxDepth_, +LoopDetector)
forward_dfs(ActionPath, State, _InterestPredicate, [], _, LoopDetector):-
    state_check_loops(State, LoopDetector, ActionPath), !.

forward_dfs(ActionPath, State, InterestPredicate, Goals, MaxDepth, LoopDetector):-
    not(state_check_loops(State, LoopDetector, ActionPath)), % This is for the cut to not change behaviour. It can be commented out.
    (call(InterestPredicate, State, ActionPath) -> 
        (GoalsIn = [ActionPath]) ; 
        (GoalsIn = [])
    ),
    (MaxDepth is 0 -> (Goals = GoalsIn) ; ( 
        MaxDepth1 is MaxDepth - 1,
        expand(ActionPath, State, NextLevel),
        state_update_loopdetector(State, ActionPath, LoopDetector, NextLoopDetector),
        forward_dfs_level(NextLevel, ActionPath, State, InterestPredicate, GoalsIn, Goals, MaxDepth1, NextLoopDetector)
    )).
