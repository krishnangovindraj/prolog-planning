:- module(planning_utils, [
    read_all_facts/2, safe_bagof/3, state_query_goal_check/3,
    skolemize/1, deskolemize/2]
    ).
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


% Skolemization, Deskolemization.
skolemize([]):- !.
skolemize([H|T]):-
    H =.. [_|ArgList],
    skolemize_args(ArgList),
    skolemize(T).

skolemize_args([]).
skolemize_args([AH|AT]):-
    not(var(AH)), !,
    skolemize_args(AT).

skolemize_args([AH|AT]):-
    var(AH),
    gensym(kgv_, VId),
    AH = kg_var(VId, _), % Introduce a new variable as well, so we can easily deskolemize
    skolemize_args(AT).


deskolemize([], []) :- !.
deskolemize([H|T], [DH|DT]):-
    H =.. [Pred|ArgList],
    deskolemize_args(ArgList, DArgList),
    DH =.. [Pred|DArgList],
    deskolemize(T, DT).

deskolemize_args([], []).
deskolemize_args([AH|AT], [AH|DT]):-
    not(AH = kg_var(_,_)), !,
    deskolemize_args(AT, DT).

deskolemize_args([AH|AT], [V|DT]):-
    AH = kg_var(_,V),% This V 
    deskolemize_args(AT, DT).