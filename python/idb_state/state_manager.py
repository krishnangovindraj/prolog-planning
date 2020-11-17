""" Naive state manager. States live in memory """
class StateManager:
    def __init__(self):
        self.state_dict = {} # state_id: int -> State

    def load_state(self, state_id):
        return self.state_dict[state_id]

    def save_state(self, state):
        self.state_dict[state.state_id] = state
        return state.state_id
