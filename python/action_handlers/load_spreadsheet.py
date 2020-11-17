from os.path import basename, splitext

from .action_handler import ActionHandler
from idb_state.state import State

from pj_protocol import JSONCompound

# TODO: avoid name collissions. Use something better than basename

class LoadSpreadsheetTask(ActionHandler):
    PREDICATE = 'load_spreadsheet'

    def handle(self):
        filename = self.action_request.action_compound.args[1]
        state = State(filename, None)
        self.state_manager.save_state(state)
        return [ JSONCompound(LoadSpreadsheetTask.PREDICATE, self.action_request.action_compound.args) ]
