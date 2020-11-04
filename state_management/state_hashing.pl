:- module(state_hashing, [update_hash/4]).

update_hash(InitialSig, DeleteList, AddList, ResultSig):-
    MOD is 1000000007,
    update_hash_delete(InitialSig, DeleteList, MOD, TempSig),
    update_hash_add(TempSig, AddList, MOD, ResultSig).
    

update_hash_delete(Sig, [], _, Sig).
update_hash_delete(InSig, [D|DT], MOD, OutSig):-
    term_hash(D, Piece),
    TempSig is (InSig + MOD - Piece) mod MOD,  
    update_hash_delete(TempSig, DT, MOD, OutSig).

update_hash_add(Sig, [], _, Sig).
update_hash_add(InSig, [A|AT], MOD, OutSig):-
    term_hash(A, Piece),
    TempSig is (InSig + Piece) mod MOD,  
    update_hash_add(TempSig, AT, MOD, OutSig).

