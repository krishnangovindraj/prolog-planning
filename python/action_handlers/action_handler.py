from typing import List, Tuple

from idb_state.idb import IDB
from pj_protocol import JSONActionRequest, AbstractJSONTerm, JSONCompound

class ActionHandler:
    class ActionPredicate:
        def to_json_compound(self):
            raise NotImplementedError("Abstract method")
        
        def __str__(self):
            return str(self.to_json_compound())

    class ErrorPredicate(ActionPredicate):
        PREDICATE = "error"
        def __init__(self, msg):
            self.msg = msg
        
        def to_json_compound(self):
            return JSONCompound(ActionHandler.ErrorPredicate.PREDICATE, [self.msg])


    def __init__(self, idb: IDB, action_request: JSONActionRequest):
        self.action_request = action_request
        self.idb = idb

    def handle(self) -> List[ActionPredicate]:
        raise NotImplementedError("Abstract method")
