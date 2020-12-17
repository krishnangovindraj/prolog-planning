:- module(synth_evaluate, []).

:- consult('evaluate/evaluate_meta.pl').
:- consult('evaluate/evaluate_tensor.pl').
:- consult('evaluate/evaluate_utils.pl').

% If I still use these terms, they mean:
% multi- A set of outputs (one-by-one)
% non-det: Multiple disjoint outputs 
% multi-non-det: multiple-disjoint sets of outputs

% TODO: Result caching layer to not make the same call over and over.
% TODO: Better argument names
