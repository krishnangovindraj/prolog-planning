from state_manager import StateManager
from pj_protocol import JSONActionRequest, JSONCompound
class ActionHandler:
    def __init__(self, state_manager: StateManager, action_request: JSONActionRequest):
        self.action_request = action_request
        self.state_manager = state_manager

    def load_state(self):
        return self.state_manager.load_state(self.action_request.state_id)

    def save_state(self, state):
        new_state_id = self.state_manager.save_state(state)
        return new_state_id


    def handle(self):
        raise NotImplementedError("Abstract method")