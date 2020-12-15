from itertools import chain as iter_chain
from idb_state.state import Tensor, Constraint

from .action_handler import ActionHandler
from .get_fields import GetFieldTypesTask

from pj_protocol import JSONActionRequest, JSONCompound, JSONList

from components.countor import learner as countor_learner
from numpy import sum as np_sum, amax as np_amax, amin as np_amin 

class FoldTensorTask(ActionHandler):
    PREDICATE = 'fold_tensor'

    agg_op_map = {
        "agg_sum": np_sum,
        "agg_max": np_amax,
        "agg_min": np_amin,
        
        # Binary data, max == or, min == and
        "agg_or": np_amax,
        "agg_and": np_amin,
    }

    class FoldTensorPredicate(ActionHandler.ActionPredicate):
        def __init__(self, tensor_id, axisI, agg_op, new_tensor_id):
            # TODO : What if they're not json lists?
            self.tensor_id = tensor_id
            self.axisI = int(axisI)
            self.agg_op = agg_op
            self.new_tensor_id = new_tensor_id

        def to_json_compound(self):
            return JSONCompound(FoldTensorTask.PREDICATE, 
                [self.tensor_id, self.axisI, self.agg_op, self.new_tensor_id])


    def handle(self):
        req = FoldTensorTask.FoldTensorPredicate(*self.action_request.action_compound.args)
        # First, get the data
        agg_fn = FoldTensorTask.agg_op_map[req.agg_op]

        tsr = self.idb.get_tensor(req.tensor_id)
        
        new_data = agg_fn(tsr.data, req.axisI)
        new_vars = [vl for i,vl in enumerate(tsr.variables) if i!=req.axisI]
        new_tsr = Tensor(new_data, new_vars, tsr.origin_table)

        self.idb.add_tensor(new_tsr)

        # print(new_tsr.data)

        return [ FoldTensorTask.FoldTensorPredicate(req.tensor_id, req.axisI, req.agg_op, new_tsr.get_id()) ]
