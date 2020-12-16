from itertools import chain as iter_chain
from idb_state.state import Tensor, Constraint

from .action_handler import ActionHandler

from pj_protocol import JSONActionRequest, JSONCompound, JSONList

from components.countor import sampler as countor_sampler

from numpy import empty as np_empty, nan as np_nan

class FillCountOrTask(ActionHandler):
    PREDICATE = 'fill_countor'

    class FillCountOrPredicate(ActionHandler.ActionPredicate):
        def __init__(self, constraint_id, fill_tensor_id, new_tensor_id):
            # TODO : What if they're not json lists?
            self.constraint_id = constraint_id
            self.fill_tensor_id = fill_tensor_id
            self.new_tensor_id = new_tensor_id
            

        def to_json_compound(self):
            return JSONCompound(FillCountOrTask.PREDICATE, [self.constraint_id, self.fill_tensor_id, self.new_tensor_id])


    def handle(self):
        req = FillCountOrTask.FillCountOrPredicate(*self.action_request.action_compound.args)
        # First, get the data
        constraint = self.idb.get_constraint(req.constraint_id)
        # base_tsr = self.idb.get_tensor(constraint.base_tensor_id)
        fill_tsr = self.idb.get_tensor(req.fill_tensor_id)

        base_tsr = self.idb.get_tensor(constraint.base_tensor_id)
        axis_lengths = [len(v) for v in base_tsr.variables] # TODO: fill_tsr.variables?
        partial_data = fill_tsr.data
        print(axis_lengths, base_tsr.data.shape, partial_data.shape )
        solutions_list = countor_sampler.generate_sample(constraint.constraint_object, axis_lengths, 1, partial_data)
        new_data = solutions_list[0]
        
        new_tsr = Tensor(new_data, fill_tsr.variables, None)
        self.idb.add_tensor(new_tsr)
        
        # TODO: Fix the encoding of the constraints
        return [ FillCountOrTask.FillCountOrPredicate(req.constraint_id, fill_tsr.get_id(), new_tsr.get_id()) ]
