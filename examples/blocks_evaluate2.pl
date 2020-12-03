% Do blocks plan through evaluate/2 & other unconventional prolog stuff.
:- use_module(state_manipulation).

planning_predicate(on(block, block)).
planning_predicate(clear(block)).

% Requirement: Any variable that appears (in the add or delete) must be part of the signature.
% NEGATION: Is a little strange. It should behave like negation-as-failure.
% Be careful and keep in mind that variables will not retain their value if they were assigned within a not.

% ^Ugly af, but the reason the DeleteList and AddList have to appear in the condition as well.

% action(action_signature, condition, effects_add, effect_delete )
e_move_to_table(X,Y, CurrentState, DeleteList, AddList):-
    state_satisfies([clear(X), on(X,Y)], CurrentState),
    DeleteList = [on(X,Y)],
    AddList = [on_table(X), clear(Y)].

e_move_from_table(X,Y, CurrentState, DeleteList, AddList):-
    state_satisfies([clear(X), on_table(X), clear(Y)], CurrentState),
    X\=Y,
    DeleteList = [on_table(X), clear(Y)],
    AddList = [on(X,Y)].


% Move X from Y to Z
e_move_block_to_block(X, Y, Z, CurrentState, DeleteList, AddList):- 
    state_satisfies([on(X,Y), clear(X), clear(Z)], CurrentState), 
        Y\=Z, X\=Z,
    DeleteList = [on(X,Y), clear(Z)],
    AddList = [on(X,Z), clear(Y)].

% Move X from Y to table
action(
    move_to_table(X,Y, DeleteList, AddList),
    [evaluate(e_move_to_table(X,Y, CurrentState, DeleteList, AddList), CurrentState)],
    DeleteList,
    AddList % Doing this breaks loop_detection: [e2(move_to_table(X,Y))|AddList]
).

% Move X from table to Y 
action(
    move_from_table(X,Y, DeleteList, AddList),
    [evaluate(e_move_from_table(X,Y, CurrentState, DeleteList, AddList), CurrentState)],
    DeleteList,
    AddList % Doing this breaks loop_detection: [e2(move_from_table(X,Y))|AddList]
).

% Move X from Y to Z
action(
    move_block_to_block(X, Y, Z, DeleteList, AddList), 
    [evaluate(e_move_block_to_block(X, Y, Z, CurrentState, DeleteList, AddList), CurrentState)],
    DeleteList,
    AddList % Doing this breaks loop_detection: [e2(move_block_to_block(X,Y,Z))|AddList]
).

constraint( [not([on(X,Y), on(Y,Z)])] ).
constraint( [clear(a)] ).

initial_state([on(a,b), on_table(b), on_table(c), clear(a), clear(c)]).


:- use_module(state_manipulation, [state_satisfies/2]).
% Having a goal check as a predicate lets us have multiple, expressive goals.
goal_check(State, _ActionPath):-
    state_satisfies([on(b,c)], State).
