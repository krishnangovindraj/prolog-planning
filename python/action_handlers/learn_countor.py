from itertools import chain as iter_chain
from idb_state.state import Table, Field

from .action_handler import ActionHandler
from .get_fields import GetFieldTypesTask

from pj_protocol import JSONActionRequest, JSONCompound, JSONList

from components.countor import learner as countor_learner
from numpy import array as np_array

class LearnCountORTask(ActionHandler):
    PREDICATE = 'learn_countor'

    class LearnCountORPredicate(ActionHandler.ActionPredicate):
        def __init__(self, table_id, axis_labels, index_map, constraint_list):
            # TODO : What if they're not json lists?
            self.table_id = table_id
            if isinstance(axis_labels, JSONList):
                self.axis_labels = [ jl.elements for jl in axis_labels.elements ]
                self.index_map = [ jl.elements  for jl in index_map.elements ]
            else:
                self.axis_labels = axis_labels
                self.index_map = index_map

            self.constraint_list = constraint_list

        def to_json_compound(self):
            return JSONCompound(LearnCountORTask.PREDICATE, [self.table_id, self.index_map, self.constraint_list])


    def handle(self):
        req = LearnCountORTask.LearnCountORPredicate(*self.action_request.action_compound.args)
        # First, get the data
        table = self.idb.get_table(req.table_id)
        # We still assume 2D. Todo: Extend it.

        records = table.get_records(include_headers=False)
        keys = [r[0] for r in records]
        variables = [keys] + req.axis_labels 
        data = self._get_data_from_indices(records, req.index_map)
        # print(data)
        
        constraints, reordered_vars = countor_learner.learnConstraints(np_array(data), variables)
        
        # TODO: Fix the encoding of the constraints
        return [ LearnCountORTask.LearnCountORPredicate(
            req.table_id, req.axis_labels, req.index_map, 
            JSONList(constraints))
        ]

        
    def _get_data_from_indices(self, records, index_map):
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
    

            