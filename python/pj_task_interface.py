from typing import Dict

from pj_protocol import JSONActionRequest, JSONResultList
from action_handlers import ActionHandler, AVAILABLE_TASKS

from idb_state.state_manager import StateManager

""" For now, this is a dummy which just a mockup of certain tasks.
Eventually we could try to build this around the existing synth-services back.
We'd also need a lot of extra endpoints to get enough info into prolog to reason.   
which contains only the requested action-predicate """
class PJTaskInterface:

    def __init__(self):
        self.state_manager = StateManager()
        

    def process_request(self, request: Dict):
        action_request = JSONActionRequest.from_dict(request)
        print(action_request)
        handler = self._resolve_handler(action_request)
        result_list = handler.handle()
        
        print([str(r) for r in result_list])
        return JSONResultList( result_list )

    
    def _resolve_handler(self, request: JSONActionRequest) -> ActionHandler:
        handler = None
        if request.action_compound.pred_name in AVAILABLE_TASKS:
            handler = AVAILABLE_TASKS[request.action_compound.pred_name](self.state_manager, request)
        else:
            raise NameError("Unknown action: " + request.action_compound.pred_name)
        return handler
