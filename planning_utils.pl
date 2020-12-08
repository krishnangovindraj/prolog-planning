:- module(planning_utils, [read_all_facts/2, safe_bagof/3, state_query_goal_check/3]).
:- use_module(state_manipulation, [state_satisfies/2]).


read_all_facts(File, FactList):-
    read(File, X) -> 
        (read(File, FC), FactList= [X|FC]);
        (FactList=[]).

safe_bagof(Template, Test, ResultList):-
    (bagof(Template, Test, ResultList) ->true ; ResultList=[]).

% Use as meta-call, pass state_query_goal_check(DesiredQuery) as InterestPredicate to the search. 
state_query_goal_check(DesiredQuery, State, _ActionPath):-
    not(not(state_satisfies(DesiredQuery, State))). % Might have to avoid unification.

