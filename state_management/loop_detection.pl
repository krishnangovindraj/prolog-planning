% Somewhat Naive but Tidy loop detection.
:- module(loop_detection, [loop_check/4]).

% If CheckOnlyHashes evaluates to true, We assume a hash collission is a loop. 
% Else we replay actions and check if they cancel out.
% loop_check(+CheckOnlyHashes, +Sig, +LoopDetector, +ActionPath)
loop_check(CheckOnlyHashes, Sig, LoopDetector, ActionPath):-
    CheckOnlyHashes -> 
        member(Sig, LoopDetector);
        full_loop_check(Sig, LoopDetector, ActionPath, [], [] ,[]).

% We apply actions and see if the effects cancel out. 
% We don't need to consider the current state, instead we just check if the positive and negative lists are equal -> They 'sum' to 0.

% This is a simplified version for documentation (I haven't tested it though)
% full_loop_check_simple(_, [], [], _, _, _):- fail.
% full_loop_check_simple(Sig, [Sig|SigTail], [Action|ActionTail], PendingActions, PositiveState, NegativeState):- 
%     apply_pending_actions([Action|PendingActions], PositiveState, NegativeState, UpdatedPositiveState, UpdatedNegativeState),
%     (UpdatedPositiveState = [], UpdatedNegativeState = []),!. % Why waste the effort of calculating the state?
   
% full_loop_check_simple(Sig, [_OtherSig|SigTail], [Action|ActionTail], PendingActions, PositiveState, NegativeState):- 
%     % Note the lack of _OtherSig \= Sig check here. In the optimized version, hash collissions don't fall through.
%     % Here they do, And it's important we accept the case Sig = OtherSig
%     full_loop_check_simple(Sig, SigTail, ActionTail, [Action|PendingActions], PositiveState, NegativeState).



% full_loop_check(+Sig, +LoopDetector, +ActionPath, +PendingActions, +PositiveState, +NegativeState).
full_loop_check(_, [], [], _, _, _):- fail, !.
full_loop_check(Sig, [Sig|SigTail], [Action|ActionTail], PendingActions, PositiveState, NegativeState):- 
    apply_pending_actions([Action|PendingActions], PositiveState, NegativeState, UpdatedPositiveState, UpdatedNegativeState), 
    !, % if do the recurse below, we can cut here. See full_loop_check_simple for the simpler version.
    % % (UpdatedPositiveState = [], UpdatedNegativeState = []),!. % Why waste the effort of calculating the state?
    (
        (UpdatedPositiveState = [], UpdatedNegativeState = [],!);
        full_loop_check(Sig, SigTail, ActionTail, [], UpdatedPositiveState, UpdatedNegativeState)
    ).

full_loop_check(Sig, [OtherSig|SigTail], [Action|ActionTail], PendingActions, PositiveState, NegativeState):- 
    OtherSig \= Sig, % The cut should take care of it.
    full_loop_check(Sig, SigTail, ActionTail, [Action|PendingActions], PositiveState, NegativeState).



% apply_pending_actions(+PendingActions, +PositiveState, +NegativeState, -UpdatedPositiveState, -UpdatedNegativeState)
apply_pending_actions([], PositiveState, NegativeState, UpdatedPositiveState, UpdatedNegativeState):-
    list_to_ord_set(PositiveState, OrdPos), list_to_ord_set(NegativeState, OrdNeg),
    ord_intersection(OrdPos, OrdNeg, OrdInt),
    ord_subtract(OrdPos, OrdInt, UpdatedPositiveState),
    ord_subtract(OrdNeg, OrdInt, UpdatedNegativeState).

apply_pending_actions([Action|PendingActions], PositiveState, NegativeState, UpdatedPositiveState, UpdatedNegativeState):-
    action(Action, _Precond, DeleteList, AddList),
    append(AddList, PositiveState, TempPositiveState),
    append(DeleteList, NegativeState, TempNegativeState),
    apply_pending_actions(PendingActions, TempPositiveState, TempNegativeState, UpdatedPositiveState, UpdatedNegativeState).

