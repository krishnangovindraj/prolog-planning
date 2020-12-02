% Not actually wrangling, but I can't spend time thinking about good names.
% :- use_module(synth_rpc).


detect_tensors(TableId, tensor(AxisLabels, IndexMap)):-
    findall(ST, synth_get_table_structure(TableId, ST), StructurePreds), 
        
    findall(FT,
        member(table_field_title(TableId, FI, FT), StructurePreds),
        FieldList),
    % TODO: Fix inefficient declarative:
    rec_header_factorize(FieldList, F1, F2),
    AxisLabels = [F1,F2], % Index map 
    create_index_map(StructurePreds, AxisLabels, IndexMap). % To test

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
    length(YList, LengthYL), LengthYL > 1, % Else it's not part of a tensor
    findall(X1, member(X1/YList, XYList) , XList_Duplicates),
    list_to_set(XList_Duplicates, XList),
    XList = [X|_], % De-duplication. The first is the representative.
    % We done.
    Factor1 = XList,
    Factor2 = YList.


% TODO: Makre recursive for more dimensions.
create_index_map(StructurePreds, HeaderAxes, IndexMap):-
    [Axis1,Axis2] = HeaderAxes,
    findall(SubMap,
        (
            member(A1, Axis1),
            findall(FI, 
                (member(A2, Axis2), member(table_field_title(_TableId, FI, [A1, A2]), StructurePreds))
                , SubMap )
        ),
        IndexMap).
