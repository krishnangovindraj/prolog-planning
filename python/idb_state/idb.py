from .state import SpreadSheet, Table

# Directory for all versions of all tables and all models
class IDB:
    def __init__(self):
        self._n_tables = 0
        self._n_models = 0
        self.spreadsheets = {}
        self.tables = {}
        self.models = {}

    def next_table_id(self) -> int:
        self._n_tables += 1
        return self._n_tables

    
    def next_model_id(self) -> int:
        self._n_models += 1
        return self._n_models

    
    def add_spreadsheet(self, ss: SpreadSheet):
        self.spreadsheets[ss._id] = ss
        ss._in_db = True 

    def get_spreadsheet(self, ss_id):
        return self.spreadsheets[ss_id]
    
    def add_table(self, tbl: Table):
        self.tables[tbl._id] = tbl
        tbl._in_db = True

    def get_table(self, tbl_id):
        return self.tables[tbl_id]
