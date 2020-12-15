from itertools import chain as iter_chain
from idb_state.state import Table, Tensor

from .action_handler import ActionHandler
from .get_fields import GetFieldTypesTask

from pj_protocol import JSONActionRequest, JSONCompound, JSONList


class TensorFromSpecTask(ActionHandler):
    PREDICATE = 'tensor_from_spec'

    class TensorFromSpecPredicate(ActionHandler.ActionPredicate):
        def __init__(self, table_id, axis_labels, index_map, tensor_id):
            # TODO : What if they're not json lists?
            self.table_id = table_id
            if isinstance(axis_labels, JSONList):
                self.axis_labels = [ jl.elements for jl in axis_labels.elements ]
                self.index_map = [ jl.elements  for jl in index_map.elements ]
            else:
                self.axis_labels = axis_labels
                self.index_map = index_map

            self.tensor_id = tensor_id

        def to_json_compound(self):
            return JSONCompound(TensorFromSpecTask.PREDICATE, [self.table_id, self.axis_labels, self.index_map, self.tensor_id])


    def handle(self):
        req = TensorFromSpecTask.TensorFromSpecPredicate(*self.action_request.action_compound.args)
        # First, get the data
        table = self.idb.get_table(req.table_id)
        # We still assume 2D. Todo: Extend it.

        tsr = Tensor.from_table_spec(table, req.axis_labels, req.index_map)
        
        self.idb.add_tensor(tsr)

        # TODO: Fix the encoding of the constraints
        return [ TensorFromSpecTask.TensorFromSpecPredicate(
            req.table_id, req.axis_labels, req.index_map, 
            tsr.get_id())
        ]
