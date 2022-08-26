% forward_dfs, but search finds all solutions at once.
% I don't think you'd ever want to use this.
% A naive but tidy generic DFS with loop detection from the state representations. 
:- module(forward_dfs, [search_forward_dfs/4]).
:- use_module(state_manipulation).

:- use_module(operations).

% search_forward_dfs(+StartState, +InterestPredicate, +MaxDepth, -Goals):-
search_forward_dfs(StartState, InterestPredicate, MaxDepth, Goals):-
    forward_dfs([], StartState, InterestPredicate, Goals, MaxDepth, []).

% ActionPath does not includes Action.
explore_branch(ActionPath, State, InterestPredicate, GoalsReached, MaxDepth, LoopDetector):-
    not(state_check_loops(State, LoopDetector, ActionPath)),
    not(violates_constraints(State, ActionPath)),

    (call(InterestPredicate, State, ActionPath) -> 
        (GoalsReached = [ActionPath|DownbranchGoals]) ; 
        (GoalsReached = DownbranchGoals)
    ),
    forward_dfs(ActionPath, State, InterestPredicate, DownbranchGoals, MaxDepth,  LoopDetector).

forward_dfs_6_bagof_body(ActionPath, State, NextLevel, InterestPredicate, MaxDepth1, NextLoopDetector, NewGoals):-
    member(NextAction, NextLevel),
    state_apply_action(State, NextAction, NextState), % In case it fails
    (
        explore_branch([NextAction|ActionPath], NextState, InterestPredicate, NewGoals, MaxDepth1, NextLoopDetector);
        not(state_cleanup(NextState, [NextAction|ActionPath])) % Runs regardless of explore_branch success/failure.
    ).
    

% forward_dfs(+ActionPath, +State, +InterestPredicate, -Goals, +MaxDepth_, +LoopDetector)
forward_dfs(_ActionPath, _State, _InterestPredicate, [], MaxDepth, _LoopDetector):-
    MaxDepth =:= 0, !. % Efficiency cut

forward_dfs(ActionPath, State, InterestPredicate, Goals, MaxDepth, LoopDetector):-
     % This is for the cut to not change behaviour. It can be commented out.
    MaxDepth =\= 0,  
    MaxDepth1 is MaxDepth - 1,
    expand(ActionPath, State, NextLevel),
    state_update_loopdetector(State, ActionPath, LoopDetector, NextLoopDetector),
    (
        bagof(NewGoals, forward_dfs_6_bagof_body(ActionPath, State, NextLevel, InterestPredicate, MaxDepth1, NextLoopDetector, NewGoals), UnflatGoals) -> 
            true;
            UnflatGoals = []
    ), % The ugly -> is for bagof failing when there are no results.
    combine_lists(UnflatGoals, Goals).

combine_lists([], []):- !.
combine_lists([H|T], F):-
    combine_lists(T, FT),
    append(H, FT, F).
