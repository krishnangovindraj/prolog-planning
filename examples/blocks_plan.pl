
planning_predicate(on(block, block)).
planning_predicate(clear(block)).

% Requirement: Any variable that appears (in the add or delete) must be part of the signature.
% NEGATION: Is a little strange. It should behave like negation-as-failure.
% Be careful and keep in mind that variables will not retain their value if they were assigned within a not.

% action(action_signature, condition, effects_add, effect_delete )

% Move X from Y to table
action(
    move_to_table(X,Y),
    [clear(X), on(X,Y)],
    [on(X,Y)],
    [on_table(X), clear(Y)]
).

% Move X from table to Y 
action(
    move_from_table(X,Y),
    [clear(X), on_table(X), clear(Y), evaluate(X\=Y)],
    [on_table(X), clear(Y)],
    [on(X,Y)]
).

% Move X from Y to Z
action(
    move_block_to_block(X, Y, Z),
    [on(X,Y), clear(X), clear(Z), evaluate(Y\=Z), evaluate(X\=Z)],
    [on(X,Y), clear(Z)],
    [on(X,Z), clear(Y)]
).

constraint( [not([on(X,Y), on(Y,Z)])] ).
constraint( [clear(a)] ).

initial_state([on(a,b), on_table(b), on_table(c), clear(a), clear(c)]).


:- use_module(state_manipulation, [state_satisfies/2]).
% Having a goal check as a predicate lets us have multiple, expressive goals.
goal_check(State, _ActionPath):-
    state_satisfies([on(b,c)], State).
