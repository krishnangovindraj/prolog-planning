
% % Hack to remove irrelevant actions from the plan sketch
hack_minimize_action_path(GoalPreds, FullPath, MinPath):- 
    % Remove any term that isn't the first occurence of a variable in goal-preds. Or itself a goal-pred.
    subset(GoalPreds, FullPath), % Branch-point: In case there are many preds that unify 
    copy_term([GoalPreds|FullPath], [PredsCopy|PathCopy]),
    hax_skolemize(PathCopy),
    create_first_occurence_map(PathCopy, _, FOMap),
    findall(V, (member(GP, PredsCopy), arg(_, GP, V), V = ez_sk(_)), GoalArgs),!,
    fo_closure(GoalArgs, FOMap, [], FOClosure),
    hack_minap(FOClosure, PredsCopy, PathCopy, FullPath, MinPath).

hack_minap(_, _, [], [], []):- !.

hack_minap(FOClosure, PredsCopy, [HC|PathCopy], [HF|FullPath], [HF|MinPath]):-
    ( member(HC, PredsCopy); member(HC, FOClosure) ),
    !,
    hack_minap(FOClosure, PredsCopy, PathCopy, FullPath, MinPath).

hack_minap(FOClosure, PredsCopy, [_HC|PathCopy], [_HF|FullPath], MinPath):-
    % not( (member(HC, PredsCopy); member(HC, FOClosure)) ),
    hack_minap(FOClosure, PredsCopy, PathCopy, FullPath, MinPath).% ,!.


hax_skolemize([]):- !.
hax_skolemize([H|T]):-
    H =.. [_|ArgList],
    hax_skolemize_args(ArgList),
    hax_skolemize(T).

hax_skolemize_args([]).
hax_skolemize_args([AH|AT]):-
    not(var(AH)), !,
    hax_skolemize_args(AT).

hax_skolemize_args([AH|AT]):-
    var(AH),
    gensym(kgv_, VId),
    AH = ez_sk(VId),
    hax_skolemize_args(AT).

create_first_occurence_map([], [], []).
create_first_occurence_map([A|PathTail], SeenVariables, FOMap):-
    create_first_occurence_map(PathTail, LaterSeenVariables, LaterFOMap),
    findall( V, (arg(_, A, V), V = ez_sk(_), not(member(V, LaterSeenVariables)) ), NewVars),
    findall( V/A, member(V, NewVars), NewMappings),
    append(NewVars, LaterSeenVariables, SeenVariables),
    append(NewMappings, LaterFOMap, FOMap).

fo_closure([], _, FOClosure, FOClosure):- !.
fo_closure(GoalVars, FOMap, Acc, FOClosure):-
    findall(
        T, 
        (member(GV, GoalVars), member(GV/T, FOMap), not(member(T, Acc)) ),
        NewTList
    ),!,
    list_to_set(NewTList, NewTSet),
    % We may now have introduced new variables from the NewTSet which aren't first occurences.
    % We need to find them and recurse.
    
    findall(V, (member(T, NewTSet), arg(_, T, V), not(member(V/T, FOMap))), NewUnseenV),
    append(NewTSet, Acc, Acc1), 
    fo_closure(NewUnseenV, FOMap, Acc1, FOClosure).
    