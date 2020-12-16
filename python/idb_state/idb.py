from .state import SpreadSheet, Table, Tensor,  Constraint

# Directory for all versions of all tables and all models
class IDB:
    def __init__(self):
        self._n_tables = 0
        self._n_models = 0
        self.spreadsheets = {}
        
        self.tables = {}
        self.tensors = {}

        self.constraints = {}
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

    def get_spreadsheet(self, ss_id) -> SpreadSheet:
        return self.spreadsheets[ss_id]
    
    def add_table(self, tbl: Table):
        self.tables[tbl._id] = tbl
        tbl._in_db = True

    def get_table(self, tbl_id) -> Table:
        return self.tables[tbl_id]

    def add_tensor(self, tsr: Tensor):
        self.tensors[tsr._id] = tsr
        tsr._in_db = True

    def get_tensor(self, tsr_id) -> Tensor:
        return self.tensors[tsr_id]

    def add_constraint(self, cstr: Constraint):
        self.constraints[cstr._id] = cstr
        cstr._in_db = True

    def get_constraint(self, cstr_id) -> Constraint:
        return self.constraints[cstr_id]

