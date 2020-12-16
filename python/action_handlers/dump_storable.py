from .action_handler import ActionHandler
from pj_protocol import JSONActionRequest, JSONCompound, JSONList

from idb_state.state import Table, Tensor, Constraint

class DumpStorableTask(ActionHandler):
    PREDICATE = "dump_storable"
    
    class DumpStorablePredicate(ActionHandler.ActionPredicate):
        def __init__(self, storable_type, storable_id):
            # TODO : What if they're not json lists?
            self.storable_type = storable_type
            self.storable_id = storable_id
            
        def to_json_compound(self):
            return JSONCompound(DumpStorableTask.PREDICATE, [self.storable_type, self.storable_id])


    def handle(self):
        req = DumpStorableTask.DumpStorablePredicate(*self.action_request.action_compound.args)
        # First, get the data
        
        if req.storable_type == "spreadsheet":
            print(self.idb.get_spreadsheet(req.storable_id).dump())
        elif req.storable_type == "table":
            print(self.idb.get_table(req.storable_id).dump())
        elif req.storable_type == "tensor":
            print(self.idb.get_tensor(req.storable_id).dump())
        elif req.storable_type == "constraint":
            print(self.idb.get_constraint(req.storable_id).dump())
        else:
            return [] # Does this cause a fail? 
        return [ req ]
