% Demonstration of POR's effectiveness - Roughly, the difference should be O(2^N) v/s O(N!)
%?- initial_state(InitialPredicates), state_create(InitialPredicates, _S),!, time( (findall(G, search_forward_dfs_por(_S, goal_check, 6, G), GL), length(GL,L)) ).
planning_predicate(on(block, block)).
planning_predicate(clear(block)).

% Move X from table to Y 
action(
    move_from_table(X,Y),
    [clear(X), on_table(X), clear(Y), evaluate(X\=Y), in_room(X,R), in_room(Y,R)],
    [on_table(X), clear(Y)],
    [on(X,Y)]
).

:- use_module(state_manipulation, [state_satisfies/2]).

initial_state([
    on_table(a), on_table(b), on_table(c), clear(a), clear(b), clear(c), in_room(a,1), in_room(b,1), in_room(c,1), 
    on_table(p), on_table(q), on_table(r), clear(p), clear(q), clear(r), in_room(p,2), in_room(q,2), in_room(r,2),
    on_table(x), on_table(y), on_table(z), clear(x), clear(y), clear(z), in_room(x,3), in_room(y,3), in_room(z,3)
    ]).

goal_check(State, _ActionPath):-
    state_satisfies([on(a,b), on(b,c), on(p,q), on(q,r), on(x,y), on(y,z)], State).
