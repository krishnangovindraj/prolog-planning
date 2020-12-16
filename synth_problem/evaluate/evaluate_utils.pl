% :- module(evaluate_utils, [no_duplicate_in_state/2]).

:- use_module(state_manipulation, [check_predicate_in_state/2]).

% Utils: Maybe I should move this to planning_utils?
% Checks if there are 2 term which match exactly with Member. (i.e., one other than member itself) 
% -, +, +
no_duplicate_in_state(Member, State):-
    % this:member/2 as  ==:=.
   copy_term(Member, MemberTemplate),
   findall( M, (
           M = MemberTemplate,
           check_predicate_in_state(M, State), % unifies
           M == Member % Checks if unification was required to make them equal.
       ), MList),
   % MLL == 0 .
   length(MList, MLL),
   MLL < 2. % < 2 if you're doing the final state version. else == 0.
   
