from os.path import isfile

from .action_handler import ActionHandler
from idb_state.state import SpreadSheet

from pj_protocol import JSONCompound, JSONErrorCompound

# TODO: avoid name collissions. Use something better than basename

class LoadSpreadsheetTask(ActionHandler):
    PREDICATE = 'load_spreadsheet'
    
    class LoadSpreadsheetPredicate(ActionHandler.ActionPredicate):
        def __init__(self, filename, spreadsheet_id):
            self.filename = filename
            self.spreadsheet_id = spreadsheet_id

        def to_json_compound(self):
            return JSONCompound(LoadSpreadsheetTask.PREDICATE, [self.filename, self.spreadsheet_id])

    def handle(self):
        # Verify it exists.
        req = LoadSpreadsheetTask.LoadSpreadsheetPredicate(*self.action_request.action_compound.args)
        filename = req.filename
        if isfile(filename):
            ss = SpreadSheet(filename, None)
            self.idb.add_spreadsheet(ss)
            return [ LoadSpreadsheetTask.LoadSpreadsheetPredicate(filename, ss.get_id()) ]
        else:
            return [ ActionHandler.ErrorPredicate("file_not_found") ] # Or do we return an error?
