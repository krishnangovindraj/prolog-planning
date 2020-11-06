:- module(planning_utils, [read_all_facts/2, query_in_list/2]).


read_all_facts(File, FactList):-
    read(File, X) -> 
        (read(File, FC), FactList= [X|FC]);
        (FactList=[]).

%% query_in_list(Query, FactList):-
query_in_list([], _).
query_in_list([evaluate(P)|T], L):-
    !, % Cut so we don't try the member
    P,
    query_in_list(T, L).

% Be careful when you use this. It's a little weird.
query_in_list([not(P)|T], L):-
    !, 
    (is_list(P),!;  writeln('ERROR: L must be a list in not(L). FIX IT!'), fail), 
    not(query_in_list(P, L)),
    query_in_list(T,L).

query_in_list([Q|T], L):-
    % not(Q = evaluate(_); Q = not(_)),
	member(Q, L),
	query_in_list(T, L).
