
% Some interaction would be nice when you don't know the result of actions.
:-use_module(representation).
:-use_module(state_manipulation).
:-use_module(planning_utils, [state_query_goal_check/3]).

:- dynamic interactive_stack_element/3. % P, StackPrev, State 

interactive_init(_ProblemFile):-
    nb_current(v_interactive_stack_top, _),!,
    writeln("ERROR: Interactive session active"),
    fail.

interactive_init(ProblemFile):-
    interactive_stack_top_set(null),
    initialize_problem(ProblemFile),
    initial_state(InitialPredicates),
    state_create(InitialPredicates, S),
    interactive_stack_push(interactive_state([],S)).

% det: + , but might fail lol.
apply_action_path(ActionPath):-
    ground(ActionPath),
    apply_action_path_do(ActionPath, ResultState),
    interactive_stack_push(ResultState). % Just to be sure.


query_current_action_path(ActionPath):-
    interactive_stack_peek(interactive_state(ActionPath, _)).

query_current_state(StateQuery):-
    is_list(StateQuery),!,
    (
        (member(X, StateQuery), var(X),!, 
        writeln("List queries cannot contain a fully variable element"), fail );
        (
            interactive_stack_peek(interactive_state(_, CurrentState)),
            state_satisfies(StateQuery, CurrentState)
        )
    ).

query_current_state(StateQuery):-
    interactive_stack_peek(interactive_state(_, CurrentState)),
    (var(StateQuery)-> 
        check_predicate_in_state(StateQuery, CurrentState); % Some safety
        state_satisfies([StateQuery], CurrentState)
    ). % ohboy


% e.g.: perform_search([on(b,c)], 4, Path).
perform_search(GoalPredicates, Depth, GoalPath):-
    perform_search_all_goals(GoalPredicates, Depth, GoalPaths),
    member(GoalPath, GoalPaths).

perform_search_all_goals(GoalPredicates, Depth, GoalPaths):-
    GoalCheck = state_query_goal_check(GoalPredicates),
    interactive_stack_peek(interactive_state(_, CurrentState)),
    search_forward_dfs(CurrentState, GoalCheck, Depth, GoalPaths).


% Private

apply_action_path_do([], CurrentState):-
    interactive_stack_peek(CurrentState).

apply_action_path_do([Action|Tail], interactive_state([Action|PathTo], ResultState)):-
    apply_action_path_do(Tail, interactive_state(PathTo, StateAt)),
    state_apply_action(StateAt, Action, ResultState).

% The interactive state stack (None of the non-interactive intermediates)?

% det: -
interactive_stack_top_get(Id):-
    nb_getval(v_interactive_stack_top, Id).

interactive_stack_top_set(Id):-
    nb_setval(v_interactive_stack_top, Id).

% det: -
interactive_stack_peek(Element):-
    interactive_stack_top_get(Id),
    interactive_stack_element(Id, _, Element).

% det: +
interactive_stack_push(Element):-
    interactive_stack_top_get(PrevId),
    gensym('istack_', NewId),
    assert(interactive_stack_element(NewId, PrevId, Element)),
    interactive_stack_top_set(NewId).
    
% det: -
interactive_stack_pop(Element):-
    interactive_stack_top_get(OldTop),
    interactive_stack_element(OldTop, NewTop, Element),
    interactive_stack_top_set(NewTop).
    
% det: -
interactive_stack_walk(Element):-
    interactive_stack_top_get(First),
    interactive_stack_walk_do(First, Element).

% det: +, -
interactive_stack_walk_do(CurrentId, Element):-
    interactive_stack_element(CurrentId, null, Element), !.


interactive_stack_walk_do(CurrentId, Element):-
    interactive_stack_element(CurrentId, PrevId, Element),
    PrevId \= null.

interactive_stack_walk_do(CurrentId, Element):-
    interactive_stack_element(CurrentId, PrevId, _),
    interactive_stack_walk_do(PrevId, Element).
