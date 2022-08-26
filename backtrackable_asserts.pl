:-module(backtrackable_asserts, [
		b_query/1,
		b_assert/1, b_asserta/1, b_assertz/1, b_retract/1, 
		nb_assert/1, nb_asserta/1, nb_assertz/1, nb_retract/1]).

:- dynamic b_asserted/2.

:- nb_setval(b_assert_nbc, 0), nb_setval(b_assert_bc, 0).


b_assert(What):-
    b_assertz(What).

b_assertz(What):-
	b_assert_maintain_consistency,
	b_getval(b_assert_nbc, NBC),
	AssertId is NBC+1,
	assertz(b_asserted(AssertId, What)),
	nb_setval(b_assert_nbc, AssertId),
	b_setval(b_assert_bc, AssertId).

% The same as b_assertz, but with asserta instead. 
b_asserta(What):-
    b_assert_maintain_consistency,
	b_getval(b_assert_nbc, NBC),
	AssertId is NBC+1,
	asserta(b_asserted(AssertId, What)),
	nb_setval(b_assert_nbc, AssertId),
	b_setval(b_assert_bc, AssertId).


b_retract(What):-
    b_assert_maintain_consistency,
    b_query_internal(Id, What),
    (
        (b_query(b_retracted(Id, What)), !);    % Already retracted
        b_assert(b_retracted(Id, What)) % Important we b_assert and not assert because the retract has to be backtracked.
    ).

b_query(What):-
	b_assert_maintain_consistency,
    b_query_internal(_, What).

b_query_internal(Id, What):-
    b_asserted(Id, What),
    not( b_asserted(_, b_retracted(Id, What)) ).

% The overhead
b_assert_maintain_consistency:-
	nb_getval(b_assert_nbc, NBC),
	b_getval(b_assert_bc, BC), % BC < NBC if backtracking happened
	b_assert_restore_consistency(BC, NBC).

b_assert_restore_consistency(C, C):- !.
b_assert_restore_consistency(BC, NBC):-
	BC1 is BC+1,
    ((
        between(BC1, NBC, DeleteMe), 
        retract(b_asserted(DeleteMe, _)),
        fail);
    true),
	nb_setval(b_assert_nbc, BC).



% nb_assert & nb_retract. Be careful & read the readme.
nb_assert(What):-
	nb_assertz(What).

nb_assertz(What):-
	% All nb_asserts go to index -1.
	assertz(b_asserted(-1, What)).

nb_asserta(What):-
	asserta(b_asserted(-1, What)).

nb_retract(What):-
	retract(b_asserted(_, What)). % The first occurence
