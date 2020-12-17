:- use_module(operations, [evaluate_predicate/1, simulate_predicate/2]).
% NO. Consult instead. % :- module(state_query, [query_state/2]).

%% query_state(Query, State):-
query_state([], _).

query_state([evaluate(P, L)|T], L):-
    !, % Cut so we don't try the member. This version has a state argument.
    operations:evaluate_predicate(P),
    query_state(T, L).

query_state([evaluate(P)|T], L):-
    !, % Cut so we don't try the member
    operations:evaluate_predicate(P),
    query_state(T, L).

% Assume perform succeeds
query_state([perform(_)|T], L):-
    !,
    query_state(T,L). 

% deprecated(query_state([evaluate(P,StateVar)|T], L)):-
%     !,
%     StateVar = L, % StateVar is bound to some variable in P, so this passes the state to the predicate. 
%     evaluate_predicate(P, L),
%     query_state(T,L).

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
