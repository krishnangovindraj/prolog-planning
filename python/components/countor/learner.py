#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 20 17:02:20 2019

@author: mohit
"""
# import helper
from . import helper
import numpy as np
from openpyxl import load_workbook
import itertools as it


def getConstraintsForAll(dataTensor, variables, orderingNotImp):
    repeatDim = ()
    r = set([v for v in range(len(variables)) if v not in repeatDim])
    constraints = {}
    for l, (m, s) in enumerate(helper.split(r, (), repeatDim)):
        newset = m + s

        # this value will be used to filter max constraints
        maxPossible = 1
        for i in range(len(s)):
            maxPossible *= len(variables[s[i]])
        idTensor = helper.tensorIndicator(dataTensor, newset, variables)

        sumSet = range(len(m), len(newset))

        sumTensor_max, sumTensor_min = helper.tensorSum(
            idTensor, sumSet, np.array(variables)[list(newset)], 0
        )

        if len(set(s)) == 1 and len(set(orderingNotImp) & set(s)) == 0:
            (
                minConsZero,
                maxConsZero,
                minConsNonZero,
                maxConsNonZero,
            ) = helper.tensorConsZero(
                idTensor, sumSet, np.array(variables)[list(newset)]
            )
        else:
            minConsZero, maxConsZero, minConsNonZero, maxConsNonZero = (0, 0, 0, 0)
        row = {}
        row["minSum"] = int(sumTensor_min) if sumTensor_min < maxPossible else 0
        row["maxSum"] = int(sumTensor_max) if sumTensor_max < maxPossible else 0
        row["minConsZero"] = int(minConsZero) if minConsZero < maxPossible else 0
        row["maxConsZero"] = int(maxConsZero) if maxConsZero < maxPossible else 0
        row["minConsNonZero"] = (
            int(minConsNonZero) if minConsNonZero < maxPossible else 0
        )
        row["maxConsNonZero"] = (
            int(maxConsNonZero) if maxConsNonZero < maxPossible else 0
        )

        key = ",".join([str(i) for i in m])
        key += ":"
        key += ",".join([str(i) for i in s])
        constraints[key] = row

    return constraints


# args: data, axis_labels
def learnConstraints(dataTensor, variables):
    # I have no idea what this is supposed to be.
    N = len(variables)
    rotate_one = [(i+1)%N for i in range(N)]
    flipped_dataTensor = dataTensor.transpose( tuple(rotate_one) )
    flipped_variables = [variables[i] for i in rotate_one]
    # print(dataTensor)
    # print(variables)
    # print(dataTensor.shape)
    
    # return 
    orderingNotImp = [0]
    constraints = getConstraintsForAll(flipped_dataTensor, flipped_variables, orderingNotImp)
    #    print(constraints)
    return constraints


#    return np.matrix([list(val.values()) for val in constraints.values()]),variables

def run_on_nurse_csv():
    def _get_data_from_indices(records, index_map):
        l1 = len(index_map)
        l2 = len(index_map[0])

        tensor = []
        for r in records:
            level = []
            for l in index_map:
                strip = []
                for idx in l:
                    strip.append( int(r[idx]) )
                level.append(strip)
            tensor.append(level)
        return tensor
    
    from numpy import array as np_array
    nurse_csv = [l.strip() for l in open("../../../synth_problem/nurse.csv") if l.strip()]
    raw = [ list(map(lambda x: x.strip(), l.split(","))) for l in nurse_csv] 

    axis_labels =  [['D0', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6'], ['S0', 'S1', 'S2']]
    index_map = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15], [16, 17, 18], [19, 20, 21]]
    
    records = raw[2:]
    variables = [[r[0] for r in records]] + axis_labels 
    
    data = np_array(_get_data_from_indices(records, index_map))
    # print(data.shape)
    cor_constraints = learnConstraints(data, variables)
    return data, cor_constraints, variables

#   constraints,var = learnConstraints("data.xlsx","sheet1",["B1:V1","B2:V2"],["A3:A14"],"B3:V14")
#   partial_sol,index_mapping=get_data("sol.xlsx","sheet1", "B3:V14",var)
#   # print(partial_sol)
#   generatesSample(len(var[1]),len(var[2]),len(var[0]),1,constraints,partial_sol,"")


if __name__ == "__main__":
    print(run_on_nurse_csv())
