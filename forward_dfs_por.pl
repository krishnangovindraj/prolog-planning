% A naive but tidy generic DFS with loop detection from the state representations
% AND NOW, DPOR!
% Once we explore the a subtree of an action fully, we set that action to sleep for any sibling subtree.
% For completeness, we must awake this action if a non-commutative action is performed (and awake it only for that subtree).
% So far in classical planning, I only see this case: If a would disable b, then a must be woken up whenever b is performed.
% Longer version at the end or in a readme-like file.

:- module(forward_dfs_por, [search_forward_dfs_por/4]).
:- use_module(state_manipulation).

:- use_module(operations).
:- use_module(por_commutativity).


% search_forward_dfs_por(+StartState, +InterestPredicate, +MaxDepth, -Goals):-
search_forward_dfs_por(StartState, InterestPredicate, MaxDepth, Goals):-
    cache_disabled_set, % Just incase it's not done yet
    % sleep_set__reset, % All sleep sets are b_asserted, hence they get reset themselves.
    forward_dfs_por([], StartState, InterestPredicate, Goals, MaxDepth, []).

% ActionPath does not includes Action.
explore_branch(ActionPath, State, InterestPredicate, Goal, MaxDepth, LoopDetector):-
    % format("explore_branch: ~w\n", [ActionPath]),
    not(state_check_loops(State, LoopDetector, ActionPath)),
    not(violates_constraints(State, ActionPath)),
    (
        (call(InterestPredicate, State, ActionPath), Goal = ActionPath);
        forward_dfs_por(ActionPath, State, InterestPredicate, Goal, MaxDepth,  LoopDetector)
    ).

forward_dfs_por_body(ActionPath, State, NextLevel, InterestPredicate, MaxDepth1, NextLoopDetector, NewGoals):-
    fdfs_por__iterate(NextAction, NextLevel),
    not(sleep_set__is_asleep(NextAction)), % Shouldn't be sleeping.
    % (sleep_set__is_asleep(NextAction) -> ( format("####~w is asleep\n", [NextAction]), fail) ;  format("- - ~w is awake\n", [NextAction])), 
    state_apply_action(State, NextAction, NextState), % In case it fails
    wake_all_disablers(NextAction),
    (
        explore_branch([NextAction|ActionPath], NextState, InterestPredicate, NewGoals, MaxDepth1, NextLoopDetector);
        not(state_cleanup(NextState, [NextAction|ActionPath])) % Runs regardless of explore_branch success/failure.
    ).

forward_dfs_por(ActionPath, State, InterestPredicate, Goal, MaxDepth, LoopDetector):-
     % This is for the cut to not change behaviour. It can be commented out.
    MaxDepth > 0,  
    MaxDepth1 is MaxDepth - 1,
    expand(ActionPath, State, NextLevel),
    state_update_loopdetector(State, ActionPath, LoopDetector, NextLoopDetector),
    forward_dfs_por_body(ActionPath, State, NextLevel, InterestPredicate, MaxDepth1, NextLoopDetector, Goal).


:- use_module(backtrackable_asserts).

sleep_set__sleep(ActionSig):-
    b_assert(sleep_set__dyn_sleeping(ActionSig)).

sleep_set__wake(ActionSig):-
    b_retract(sleep_set__dyn_sleeping(ActionSig)), !. % The retract seems to be causing backtrack points

sleep_set__is_asleep(ActionSig):-
    b_query(sleep_set__dyn_sleeping(ActionSig)).

% member, but puts executed ones to sleep
fdfs_por__iterate(Next, [Next]):- !.

fdfs_por__iterate(Next, [Next|_]).

fdfs_por__iterate(Next, [H|T]):-
    sleep_set__sleep(H),
    fdfs_por__iterate(Next, T).


wake_all_disablers(PerformedActionSig):-
    findall(DA, (query_disabler_actions(PerformedActionSig, DA), sleep_set__is_asleep(DA)), DisablerActionList),
    % What if the same action matched multiple templates? De-duplicate.
    list_to_ord_set(DisablerActionList, DisablerActionSet),
    % format("\t ~w wakes ~w\n", [PerformedActionSig, DisablerActionSet]),
    wake_all_disablers__wake_list(DisablerActionSet).

wake_all_disablers__wake_list([]):- !.
wake_all_disablers__wake_list([H|T]):-
    sleep_set__wake(H),
    wake_all_disablers__wake_list(T).


% tl;dr of dpor: If a,b are commutative, the plans <X,a,b,Y> and <X,b,a,Y> are equivalent.
%   To prevent this, suppose you have finished exploring at <X,a>. This means you have seen <X,a,b,*> 
% You now put a to sleep. As long as a is asleep, it will never occur any following schedule  <X,*>
% For a simple example, it means for 3 actions [a,b,c], you'd only execute, [a,b,c], [a,c], [b,c], [c] if a,b,c were all commutative.
% The complication occurs when a,b are not commutative.
%   Since actions either add or subtract from a set, these operations being commutative guarantee that the actions themselves would be commutative, should they execute.
% Hence, the only non-commutativity (that I'm aware of, atleast) is when an action a disables b. In such a case, in <X,a,Y>, Y will never contain b.
% If we put a to sleep, <X,b,*,a,*> will never be executed. Which leads to incompleteness. Hence we must wake a up for the subtree of b.
