:- module(planning_utils, [read_all_facts/2, query_in_list/2]).


read_all_facts(File, FactList):-
    read(File, X) -> 
        (read(File, FC), FactList= [X|FC]);
        (FactList=[]).

%% query_in_list(Query, FactList):-
query_in_list([], _).
query_in_list([evaluate(P)|T], L):-
    P,
    query_in_list(T, L).

query_in_list([Q|T], L):-
	member(Q, L),
	query_in_list(T, L).
