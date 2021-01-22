% Takes ~ 3 minutes to run on my machine. (blocks_por_rooms is easier to run)
% ?- initial_state(InitialPredicates), state_create(InitialPredicates, _S),!,time( (findall(G, search_forward_dfs_por(_S, goal_check, 6, G), GL), length(GL,L)) ).

planning_predicate(on(block, block)).
planning_predicate(clear(block)).

% Move X from table to Y 
action(
    move_from_table(X,Y),
    [clear(X), on_table(X), clear(Y), evaluate(X\=Y)],
    [on_table(X), clear(Y)],
    [on(X,Y)]
).

:- use_module(state_manipulation, [state_satisfies/2]).

initial_state([
    on_table(a), on_table(b), on_table(c), clear(a), clear(b), clear(c), 
    on_table(p), on_table(q), on_table(r), clear(p), clear(q), clear(r),
    on_table(x), on_table(y), on_table(z), clear(x), clear(y), clear(z)
    ]).

goal_check(State, _ActionPath):-
    state_satisfies([on(a,b), on(b,c), on(p,q), on(q,r), on(x,y), on(y,z)], State).
