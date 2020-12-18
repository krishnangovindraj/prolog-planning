:- module(evaluate_wrangling, [synth_detect_tensors_impl/4]).

:- use_module(state_manipulation).
% Not actually wrangling, but I can't spend time thinking about good names.



% +, +, -, -
synth_detect_tensors_impl(TableId, FieldHeaderList, AxisLabels, IndexMap):-
    % TODO: Fix inefficient declarative:
    findall(FT,
        member(field_header(TableId, _FI, FT), FieldHeaderList),
        FieldHeaderNamesList),
    rec_header_factorize(FieldHeaderNamesList, F1, F2),
    AxisLabels = [F1,F2], % Index map 
    create_index_map(FieldHeaderList, AxisLabels, IndexMap). % To test

% TODO: Make recursive so we can do more than 3 dimensions                                                                                                                           
% And maybe we can avoid the false at the end?
% +, -, - (for now)
rec_header_factorize(HeaderList, Factor1, Factor2):-
    findall(
        X/YList,(
            member([X,_], HeaderList),
            findall(Y, member([X,Y], HeaderList), YList)
        ), XYList_Duplicates
    ), % XYList = [ 1/[a,b,c], 2/[a,b,c], ...]
    
    list_to_set(XYList_Duplicates, XYList),
    % Jesus, the variable namming is trouble. But only XYList is bound at this point
    
    % Anyway, the other direction.
    member(X/YList, XYList),
    length(YList, LengthYL), % LengthYL > 1, % Else it's not part of a tensor % HACK: TODO: UNCOMMENT?!
    findall(X1, member(X1/YList, XYList) , XList_Duplicates),
    list_to_set(XList_Duplicates, XList),
    XList = [X|_], % De-duplication. The first is the representative.
    % We done.
    Factor1 = XList,
    Factor2 = YList.


% TODO: Makre recursive for more dimensions.
create_index_map(FieldHeaderList, HeaderAxes, IndexMap):-
    [Axis1,Axis2] = HeaderAxes,
    findall(SubMap,
        (
            member(A1, Axis1),
            findall(FI, 
                (member(A2, Axis2), member(field_header(_TableId, FI, [A1, A2]), FieldHeaderList))
                , SubMap )
        ),
        IndexMap
    ).





synth_tensor_fold_meta_impl(OriginalShape, FoldAxis, NewShape):-
    nth0(FoldAxis, OriginalShape, _),
    replace_nth(FoldAxis, 1, OriginalShape, NewShape).

replace_nth(0, WithWhat, [_|T], [WithWhat|T]):- !.

replace_nth(N, WithWhat, [H|T], [H|DT]):-
    N>0, 
    N1 is N-1,
    replace_nth(N1, WithWhat, T, DT).
