:- module(json_client, [query_synth/1]).
% Code which lets us interface with python functions.


:- use_module(library(http/http_open)).     % For the actual http_open call.
:- use_module(library(http/http_json)).     % Includes the post_data_hook that enables json support. 
:- use_module(library(http/json_convert)).  % For easy conversion between terms & json

% Couple of settings:
backend_url('http://127.0.0.1:8001').


% Our protocol terms
:- json_object
    json_compound(list_rep: list) + [type=json_compound],
    json_list(elements: list) + [type=json_list],
    json_action_request(action: json_compound/1) + [type=json_action_request],
    json_result_list(results: list) + [type=json_result_list].


query_synth(QueryTerm):-
    backend_url(Url),
    hit_api(Url, QueryTerm, ResultList),
    member(QueryTerm, ResultList).

% TODO: Handle errors.
% Example usage (And test):  Action = test_protocol([nested(foo(s0,s1)),nested(s2)], x_), query_synth("http://127.0.0.1:8001", Action, RL), RL = [Action].
% hit_api(+Url, +ActionRequest, -ResultList)
hit_api(Url, ActionRequest, ResultList):-
    parse_ptoj(ActionRequest, ActionRequestObject),
    prolog_to_json(json_action_request(ActionRequestObject), JsonActionRequest),
    
    RequestOptions = [post(json(JsonActionRequest))], % [size(S)] % extra stuff not needed.
    http_open(Url, RespStream, RequestOptions), 
    json:json_read(RespStream, JsonResponse), close(RespStream),

    json_to_prolog(JsonResponse, json_result_list(ResultListObject)),
    maplist(parse_jtop, ResultListObject, ResultList).


% Extra layer to convert between protocol terms and arbitrary terms.
% parse_jtop(+JsonObject, -Term).
parse_jtop(json_list(JsonListElements), Term):-
    !,
    maplist(parse_jtop, JsonListElements, Term). 

parse_jtop(json_compound([Pred|RawArgList]), Term):-
    !,
    maplist(parse_jtop, RawArgList, ArgList),
    Term =.. [Pred|ArgList]. 
    
parse_jtop(T,T):-
    not(T = json_compound(_)).

% parse_ptoj(+Term, -JsonObject).
parse_ptoj(T, json_list(JsonList)):-
    is_list(T), !,
    maplist(parse_ptoj, T, JsonList).

parse_ptoj(T, json_compound(L)):-
    compound(T), not(is_list(T)), !,
    T =.. RawList,
    maplist(parse_ptoj, RawList, L).

parse_ptoj(T,var__):-
    not(compound(T)), var(T), !.

parse_ptoj(T,T):-
    not(compound(T)), not(var(T)).
