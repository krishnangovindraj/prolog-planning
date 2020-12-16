:- module(interactive, [interactive_init/1, one_iter/0,
    get_current_action_path/1, query_current_state/1, apply_action_path/1, apply_action_path/2,
    perform_search/3, perform_search_all_goals/3]).
% Some interaction would be nice when you don't know the result of actions.
:-use_module(operations).
:-use_module(state_manipulation).
:-use_module(planning_utils, [state_query_goal_check/3]).
:-use_module(forward_dfs).

:- dynamic interactive_stack_element/3. % P, StackPrev, State 

one_iter:-
    interactive:get_current_state(S),
    interactive:get_applicable_action(AL),
    member(A,AL),
    is_yn_prompt("Sketch action:\n\t ~k", [A]),
    operations:evaluate_plan_sketch([A],S, _F), is_yn_prompt("Apply Action:\n\t ~k", [A]),
    interactive:apply_action_path([A]).

is_load(X):-
    nb_getval(is_stored_val, X).
is_store(X):-
    nb_setval(is_stored_val, X).

is_yn_prompt(FormatStr, FormatArgs):-
    format(FormatStr, FormatArgs), nl,
    get_single_char(121). % y

interactive_init(_InitialStatePredicateList):-
    nb_current(v_interactive_stack_top, _),!,
    writeln("ERROR: Interactive session active"),
    fail.

interactive_init(InitialStatePredicateList):-
    nb_setval(v_interactive_stack_top, null),
    state_create(InitialStatePredicateList, S),
    interactive_stack_push(interactive_state([],S)).

% det: + , but might fail lol.
get_current_action_path(ActionPath):-
    interactive_stack_peek(interactive_state(ActionPath, _)).


get_current_state(State):-
    interactive_stack_peek(interactive_state(_, State)).


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


get_applicable_action(Action):-
    interactive_stack_peek(interactive_state(CurrentActionPath, CurrentState)),
    expand(CurrentActionPath, CurrentState, Action).

apply_action_path(ActionPath):-
    apply_action_path(ActionPath, _).

apply_action_path(ActionPath, ResultState):-
    ground(ActionPath),
    apply_action_path_do(ActionPath, ResultState),
    interactive_stack_push(ResultState). % Just to be sure.
    

% e.g.: perform_search([on(b,c)], 4, Path).
perform_search(GoalPredicates, Depth, GoalPath):-
    perform_search_all_goals(GoalPredicates, Depth, GoalPaths),
    member(GoalPath, GoalPaths).

perform_search_all_goals(GoalPredicates, Depth, GoalPaths):-
    GoalCheck = planning_utils:state_query_goal_check(GoalPredicates),
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
