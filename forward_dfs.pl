% A naive but tidy generic DFS with loop detection from the state representations. 
:- module(forward_dfs, [search_forward_dfs/4]).
:- use_module(state_manipulation).

:- use_module(representation).

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

% forward_dfs(+ActionPath, +State, +InterestPredicate, -Goals, +MaxDepth_, +LoopDetector)
forward_dfs(_ActionPath, _State, _InterestPredicate, [], MaxDepth, _LoopDetector):-
    MaxDepth =:= 0, !. % Efficiency cut

forward_dfs(ActionPath, State, InterestPredicate, Goals, MaxDepth, LoopDetector):-
     % This is for the cut to not change behaviour. It can be commented out.
    MaxDepth =\= 0,  
    MaxDepth1 is MaxDepth - 1,
    expand(ActionPath, State, NextLevel),
    state_update_loopdetector(State, ActionPath, LoopDetector, NextLoopDetector),
    findall(Goals, 
        (
            member(NextAction, NextLevel),
            state_apply_action(State, NextAction, NextState), % In case it fails
            (
                explore_branch([NextAction|ActionPath], NextState, InterestPredicate, Goals, MaxDepth1, NextLoopDetector);
                not(state_cleanup(NextState, [NextAction|ActionPath])) % Runs regardless of explore_branch success/failure.
            )
        ),
        UnflatGoals
    ),
    combine_lists(UnflatGoals, Goals).

combine_lists([], []):- !.
combine_lists([H|T], F):-
    combine_lists(T, FT),
    append(H, FT, F).
