from enum import Enum
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
        self.records = []
        self.fields = []

    
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
    
    class DataTypes(Enum):
        DT_BLANK = 0    # Blank. UNKNOWN is states = None anyway.  
        
        DT_REAL = 1     # 
        DT_INT  = 2     #
        DT_STR  = 8     # 
        DT_BOOL = 16     # 0/1 or T, F or True, False
        
        DT_CAT = 32     # Limited cardinality int/str. Foreign keys are also this for now.
        DT_ID = 64      # Primary key. Unique INT/STR
        
        # Shorthand unions for programmer convenience
        DT_NUMERIC = 3   # Handy union for me
