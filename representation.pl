:- module(representation, [
    load_problem/1,
    action/5, initial_state/1, goal_check/2, constraint/1]).

:- use_module(problem, []).

load_problem(ProblemFile):-
    problem:do_consult_problem(ProblemFile).

% action(ActionSig, PreCond, DeleteList, AddList, PerformList):-
%     problem:action(ActionSig, PreCond, DeleteList, AddList, PerformList).

action(ActionSig, PreCond, DeleteList, AddList, []):-
    problem:action(ActionSig, PreCond, DeleteList, AddList).

initial_state(S):-
    problem:initial_state(S).

goal_check(ActionPath, State):-
    problem:goal_check(ActionPath, State).

constraint(C):- 
    problem:constraint(C).
