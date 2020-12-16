from idb_state.state import Tensor, Constraint

from .action_handler import ActionHandler

from pj_protocol import JSONActionRequest, JSONCompound, JSONList

from components.countor import sampler as countor_sampler

from numpy import empty as np_empty, nan as np_nan

class GenerateCountORTask(ActionHandler):
    PREDICATE = 'generate_countor'

    class GenerateCountORPredicate(ActionHandler.ActionPredicate):
        def __init__(self, constraint_id, new_tensor_id):
            # TODO : What if they're not json lists?
            self.constraint_id = constraint_id
            self.new_tensor_id = new_tensor_id
            

        def to_json_compound(self):
            return JSONCompound(GenerateCountORTask.PREDICATE, [self.constraint_id, self.new_tensor_id])


    def handle(self):
        req = GenerateCountORTask.GenerateCountORPredicate(*self.action_request.action_compound.args)
        # First, get the data
        constraint = self.idb.get_constraint(req.constraint_id)
        base_tsr = self.idb.get_tensor(constraint.base_tensor_id)
        axis_lengths = [len(v) for v in base_tsr.variables]
        
        partial_data = np_empty( axis_lengths ); partial_data[:] = np_nan # TODO
        solutions_list = countor_sampler.generate_sample(constraint.constraint_object, axis_lengths, 1, partial_data)
        new_data = solutions_list[0]
        # print("SHAPES: ", base_tsr.data.shape, new_data.shape)
        
        new_tsr = Tensor(new_data, base_tsr.variables, None)
        self.idb.add_tensor(new_tsr)
        
        # TODO: Fix the encoding of the constraints
        return [ GenerateCountORTask.GenerateCountORPredicate(req.constraint_id, new_tsr.get_id()) ]
