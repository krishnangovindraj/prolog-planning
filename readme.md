## tl;dr
Toy prolog planner to play with simple planning problems. 
I read "Artificial Intelligence: A Modern Approach", but there's no regression planning like STRIPS in here (yet). The ideas of having an intial state, a goal-check and a set of actions (with pre-conditions, Delete lists, Add lists) stay and are used in the forward dfs. The nice quirk: Preconditions can be callable prolog predicates, (subject to conditions). See ["Blocks example"](#Blocks-Example)

This cannot be used for serious planning problems. If that's what you need, Use a real solver.

## Components
* A declarative problem representation based on action/4:
    `action(ActionSignature, PreconditionList, DeleteList, AddList).`    
* A lazy DFS with a depth-limit (and sometimes loop-detection), to do forward planning. 
* A really inefficient way of supporting constraints on the state 
    - by literally querying the state against the constraints after every action.
* (in-progress) A minimal and (hopefully) easy way to explore plans which need some interaction, so you don't have to trace/spy. 
* A way to call python functions from prolog using an http server and JSON.   
    - Should be easy to adapt for your own needs, but it may need some tuning.
    - See `/json_client.pl` and `/python/*.py` for more.
    - I should rework this to convert things to problog/pylo terms in the python space. 


## YOU'LL PROBABLY HAVE A BAD TIME IF YOU DON'T READ THIS:
### On simple programs with no variables in the state ###
* The initial state ***MUST*** be ground.
* Any variable appearing in the Add/Delete List ***MUST*** appear in the ActionSignature
* All variables in an evaluate must be ground post evaluation OR not appear in the Delete/AddList.
These together should guarantee that any state will be ground by construction.

If/When I (or you?) sit down to finally write a backwards planner, I will hopefully allow variables to be in the state. I don't even know if that's possible, but for now I have another approach:

### On programs with perform/variables ###
- Performs ***MUST FAIL*** if they do not meet their promises.
- If following actions make no assumptions about the value of the variables (this includes structure, which is complicated for lists), you should be fine.
- All variables must be ground after a perform is performed.

## Blocks Example
`/examples/blocks_plan.pl` contains a simple planning problem in the block world (from AI:AMA), You can see how the actions are declared.

`/examples/blocks_evaluate2.pl` shows how the same problem is declared with callable prolog.


### Running the examples.
**Running the whole search at once**: 
    
    $ prolog testbed.pl
    ?- representation:initialize_problem('examples/blocks_plan.pl').
    ?- initial_state(InitialPredicates), state_create(InitialPredicates, _S),!, 
            search_forward_dfs(_S, goal_check, 4, GL).
        InitialPredicates = [on(a, b), on_table(b), on_table(c), clear(a), clear(c)],
        _S = assert_state_d(103, meta(56283180, 5)), % Depends on the state-rep in state_manipulation.pl
        GL = [[move_from_table(b, c), move_to_table(a, b)], [move_from_table(b, c), move_to_table(a, c), move_block_to_block(a, b, c)]]. % The goal list, yay!

**Using the interative file**: 
    $ prolog testbed.pl
    ?- use_module(interactive).
    ?- representation:initialize_problem('examples/blocks_plan.pl').
    ?- initial_state(SL), interactive_init(SL).
        true. % HIT ENTER!!!

    ?- query_current_state(P). % Wanna see the state?
        P = on(a, b) ;
        P = on_table(b) ;
        P = on_table(c) ;
        P = clear(a) ;
        P = clear(c).

    ?- perform_search([on(b,c)], 4, P).
        P = [move_from_table(b, c), move_to_table(a, b)] ;
        P = [move_from_table(b, c), move_to_table(a, c), move_block_to_block(a, b, c)].

    ?- apply_action_path([move_from_table(b, c), move_to_table(a, b)]).
        true . % HIT ENTER!!! I'll sort this out later.

    ?- query_current_state(P). % See, it changed :)
        P = on_table(c) ;
        P = clear(a) ;
        P = on_table(a) ;
        P = clear(b) ;
        P = on(b, c).
