:- module(problem, [do_consult_problem/1]).

:- dynamic 
    action/4, action/5, initial_state/1, goal_check/2, constraint/1,
    perform/1.

% This file exists so I can keep the problem separate and define an interface through representation.pl
do_consult_problem(ProblemFile):-
    consult(ProblemFile).

perform_perform(perform(Perform, State), State):-
    perform(Perform). % One of the peform's arguments is the State

perform_perform(perform(Perform), _):-
    perform(Perform).

evaluate_predicate(Predicate):- 
    Predicate. % call
