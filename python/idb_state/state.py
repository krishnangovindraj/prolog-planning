from enum import IntEnum
from typing import List, Tuple
from copy import deepcopy
from os.path import basename, splitext
# NOT THREAD-SAFE
# TODO: Uncomment the line which increments state_id :p 
# TODO: Rewrite the whole thing
# TODO: (Low hanging), write lazy deep_copies for these types.  
""" Simplest mockups """
class Storable:
    def __init__(self, provisional_id: str, _in_db: bool = False ):
        self._in_db = _in_db
        self._id = provisional_id

    def get_id(self):
        return self._id if self._in_db else None

class CellRange:
    def __init__(self, filename, row_start, col_start, n_rows, n_cols): # sheet_name, 
        self.filename = filename
        # self.sheet_name = sheet_name
        self.row_start = row_start
        self.col_start = col_start
        self.n_rows = n_rows
        self.n_cols = n_cols


# The storable classes
class SpreadSheet(Storable):

    SS_ID_AI = 0
    def __init__(self, filename:str, parent_ss: 'SpreadSheet'):
        super(SpreadSheet, self).__init__(SpreadSheet.get_next_id(filename))
        self.filename = filename
        self.parent_spreadsheet = parent_ss    # Can be null
        # self.sheets = {}    # spreadsheet key -> filename maybe?
        # self.tables = None # List[str]    # Str are table_ids
        self.id = None # No ID till it's saved

    @staticmethod
    def get_next_id(filename):
        SpreadSheet.SS_ID_AI += 1            # Not thread-safe
        return 'ss_' + splitext(basename(filename))[0] + '_' + str(SpreadSheet.SS_ID_AI)

    # @staticmethod
    # def clone_parent(parent_ss: 'SpreadSheet') -> 'SpreadSheet':
    #     new_ss = SpreadSheet(parent_ss.filename, parent_ss)
    #     # new_ss.tables = None if parent_ss.tables is None else [Table.from_table(t) for t in parent_ss.tables ] # Do I copy or what? For now I copy. Later, we make lazy copy.
    #     # new_ss.models = [m for m in parent_ss.models] # Creates a new list so we can add models without changing the original. Again, may want to do lazy.
    #     return new_ss


"""
Assumes records are in rows, colums are fields
    row_range: (start_row: int, end_row: int)
    col_range: (start_col: int, n_cols: int)
"""
class Table(Storable):

    TBL_ID_AI = 0
    def __init__(self, origin_location: CellRange, parent: 'Table'):
        super(Table, self).__init__(Table.get_next_id())
        self.origin = origin_location 
        self.records = None
        self.fields = None
        self.header_rows = None
    
    def get_records(self, include_headers=True):
        if include_headers:
            return [r for r in self.records]
        else:
            return [r for i,r in enumerate(self.records) if i not in self.header_rows ]

    def n_rows(self):
        return len(self.records)
    
    def n_cols(self):
        return max([len(r) for r in self.records])

    @staticmethod
    def get_next_id():
        Table.TBL_ID_AI += 1            # Not thread-safe
        return 'tbl_' + str(Table.TBL_ID_AI)

    # Easy creation from earlier 
    @staticmethod
    def from_table(src: 'Table') -> 'Table':
        new_table = Table(src.origin, src)
        new_table.records = [ [v for v in r]  for r in src.records]
        new_table.fields = [f.clone() for f in src.fields] # New list -> can add/remove fields.  deep copy ONLY because of missing :(. Again, lazy copies would be cool
        return new_table
    
    def __str__(self):
        return "Table[(%d,%d),(%d,%d)]"%(
            self.row_range[0], self.row_range[0] + self.row_range[1],
            self.col_range[0], self.col_range[0] + self.col_range[1],
        )

class Tensor(Storable):
    
    TSR_ID_AI = 0
    @staticmethod
    def get_next_id():
        Tensor.TSR_ID_AI += 1            # Not thread-safe
        return 'tsr_' + str(Tensor.TSR_ID_AI)

    def __init__(self, data, variables:List[List[str]], origin_table: Table):
        super(Tensor, self).__init__(Tensor.get_next_id())
        self.data  = data
        self.variables = variables
        self.shape = [len(vl) for vl in variables]
        self.origin_table = origin_table
        self.data_type = "todo"

    # Easy creation from earlier 
    @staticmethod
    def from_table_spec(table: 'Table', axis_labels, index_map) -> 'Table':
        from numpy import array as np_array

        records = table.get_records(include_headers=False)
        keys = [r[0] for r in records]
        variables = [keys] + axis_labels 
        data = np_array(Tensor._get_data_from_indices(records, index_map))
        tsr = Tensor(data, variables, table)

        return tsr
    
    @staticmethod
    def _get_data_from_indices(records, index_map):
        l1 = len(index_map)
        l2 = len(index_map[0])

        tensor = []
        for r in records:
            level = []
            for l in index_map:
                strip = []
                for idx in l:
                    strip.append( int(r[idx]) )
                level.append(strip)
            tensor.append(level)
        return tensor

    @staticmethod
    def pad_to_size_3(data, vars):
        if len(data.shape) < 3:            
            from numpy import reshape as np_reshape
            next_data = np_reshape(data, (1,) + data.shape)
            next_variables = [["dummy"]] + variables
            return Tensor.pad_to_size_3(next_data, next_variables)
        else:
            return data, vars

    def __str__(self):
        return "Tensor(%s,%s)]"%(
            self.data_type, self.shape
        )

class Constraint(Storable):
    CSTR_ID_AI = 0
    @staticmethod
    def get_next_id():
        Constraint.CSTR_ID_AI += 1            # Not thread-safe
        return 'cstr_' + str(Constraint.CSTR_ID_AI)

    def __init__(self, constraint_type: str, constraint_object, base_tensor_id):
        super(Constraint, self).__init__(Constraint.get_next_id())
        self.constraint_type = constraint_type
        self.constraint_object = constraint_object
        self.base_tensor_id = base_tensor_id
    

class Record:
    def __init__(self, values: List, is_header: bool):
        self.values = values
        self.is_header = is_header

    def clone(self):
        return deepcopy(self)


class Field:
    def __init__(self, table: Table, label: str):
        self.table = table
        self.label = label
        # self.meta = {}        # Maybe I should make this a bunch of static fields
        self.has_missing = None
        self.data_types = None  # Candidate data types

    def clone(self):
        return deepcopy(self)
    
    class DataTypes(IntEnum):
        DT_BLANK = 0    # Blank. UNKNOWN is states = None anyway.  
        
        DT_REAL = 1     # 
        DT_INT  = 2     #
        DT_STR  = 8     # 
        DT_BOOL = 16     # 0/1 or T, F or True, False
        
        DT_CAT = 32     # Limited cardinality int/str. Foreign keys are also this for now.
        DT_ID = 64      # Primary key. Unique INT/STR
        
        # Shorthand unions for programmer convenience
        DT_NUMERIC = 3   # Handy union for me
