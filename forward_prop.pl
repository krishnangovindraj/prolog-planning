:- use_module(planning_utils).
:- dynamic action/4.

%% forward_dfs_level(LevelActions, ActionPath, InterestPredicate, FoundGoals, UpdatedGoals)

%% Move goal check to forward_dfs
% forward_dfs_level([], ActionPath, State, InterestPredicate, GoalAccIn, GoalAccOut, _MaxDepth):-
%     (call(InterestPredicate, State, ActionPath) -> 
%         (GoalAccOut = [ActionPath|GoalAccIn]) ; 
%         (GoalAccOut = GoalAccIn)
%     ).
forward_dfs_level([], _ActionPath, _State, _InterestPredicate, GoalAcc, GoalAcc, _MaxDepth).
forward_dfs_level([NextAction|SiblingActions], ActionPath, State, InterestPredicate, GoalAccIn, GoalAccOut, MaxDepth):-
    % Recurse - 
    (apply_action(State, NextAction, ResultState) ->  % In case it fails
        forward_dfs([NextAction|ActionPath], ResultState, InterestPredicate, BranchGoals, MaxDepth); 
        BranchGoals = []
    ),
    % Process remaining children
    append(BranchGoals, GoalAccIn, GoalAccTemp),
    forward_dfs_level(SiblingActions, ActionPath, State, InterestPredicate, GoalAccTemp, GoalAccOut, MaxDepth).

% forward_dfs(_,_,_,[], 0):- !. % More elegant way of doing it.
forward_dfs(ActionPath, State, InterestPredicate, Goals, MaxDepth):-
    (call(InterestPredicate, State, ActionPath) -> 
        (GoalsIn = [ActionPath]) ; 
        (GoalsIn = [])
    ),
    (MaxDepth is 0 -> (Goals = GoalsIn) ; ( 
        MaxDepth1 is MaxDepth - 1,
        expand(ActionPath, State, NextLevel),
        forward_dfs_level(NextLevel, ActionPath, State, InterestPredicate, GoalsIn, Goals, MaxDepth1)
    )).

expand(_ActionPath, State, Children):-
    % Find potential actions right?
    findall(ActionSig, (action_signature(ActionSig), is_applicable_action(ActionSig, State)), Children).



 % Produces every applicable action signature. Will be ground if State is ground.
is_applicable_action(ActionSig, State):-
    action(ActionSig, Preconditions, _, _),
    query_in_list(Preconditions, State).

action_signature(AS):-
    action(AS, _, _, _).

apply_action(State, Action, ResultState):-
    action(Action, _Precond, Delete, Add),
    subtract(State, Delete, TempState),
    append(Add, TempState, ResultState).