
% NO. Consult instead. % :- module(state_query, [query_state/2]).

%% query_state(Query, State):-
query_state([], _).
query_state([evaluate(P)|T], L):-
    !, % Cut so we don't try the member
    P, % evaluate_predicate(P,L),
    query_state(T, L).

% Be careful when you use this. It's a little weird.
query_state([not(P)|T], L):-
    !, 
    (is_list(P),!;  writeln('ERROR: L must be a list in not(L). FIX IT!'), fail), 
    not(query_state(P, L)),
    query_state(T,L).

query_state([Q|T], L):-
    % not(Q = evaluate(_); Q = not(_)),
	check_predicate_in_state(Q, L), % This comes from the state representation.
	query_state(T, L).
