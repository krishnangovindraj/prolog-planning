:- module(planning_utils, [read_all_facts/2]).

read_all_facts(File, FactList):-
    read(File, X) -> 
        (read(File, FC), FactList= [X|FC]);
        (FactList=[]).
