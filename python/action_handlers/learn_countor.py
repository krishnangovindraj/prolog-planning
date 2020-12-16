from itertools import chain as iter_chain
from idb_state.state import Tensor, Constraint

from .action_handler import ActionHandler

from pj_protocol import JSONActionRequest, JSONCompound, JSONList

from components.countor import learner as countor_learner

class LearnCountORTask(ActionHandler):
    PREDICATE = 'learn_countor'

    class LearnCountORPredicate(ActionHandler.ActionPredicate):
        def __init__(self, tensor_id, constraint_id):
            # TODO : What if they're not json lists?
            self.tensor_id = tensor_id
            
            self.constraint_id = constraint_id

        def to_json_compound(self):
            return JSONCompound(LearnCountORTask.PREDICATE, [self.tensor_id, self.constraint_id])


    def handle(self):
        req = LearnCountORTask.LearnCountORPredicate(*self.action_request.action_compound.args)
        # First, get the data
        
        tsr = self.idb.get_tensor(req.tensor_id)    
        tsr_data, tsr_variables = Tensor.pad_to_size_3(tsr.data, tsr.variables)

        cor_constraints = countor_learner.learnConstraints(tsr_data, tsr_variables)

        constraint = Constraint('countor', cor_constraints, tsr.get_id())

        self.idb.add_constraint(constraint)

        # TODO: Fix the encoding of the constraints
        return [ LearnCountORTask.LearnCountORPredicate(req.tensor_id, constraint.get_id()) ]
