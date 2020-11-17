from enum import Enum
from typing import List, Tuple
from copy import deepcopy
# NOT THREAD-SAFE
# TODO: Uncomment the line which increments state_id :p 
# TODO: Rewrite the whole thing
# TODO: (Low hanging), write lazy deep_copies for these types.  
""" Simplest mockups """
class State:
    STATE_ID_AI = 0 # NOT THREAD-SAFE

    def __init__(self, filename:str, parent_state: 'State'):
        # State.STATE_ID_AI += 1            # Not thread-safe
        self.state_id = State.STATE_ID_AI 
        self.filename = filename
        self.parent_state = parent_state    # Can be null
        # self.sheets = {}    # spreadsheet key -> filename maybe?
        self.tables = None # List[Table]    # None means we don't know
        self.models = [] # List[Model]      # amidoinitrite? (it = Inductive Databases).
        # Let's say constraints are stored in a constraint model or something.

    @staticmethod
    def clone_parent(parent_state: 'State') -> 'State':
        new_state = State(parent_state.filename, parent_state)
        new_state.tables = None if parent_state.tables is None else [Table.from_table(t) for t in parent_state.tables ] # Do I copy or what? For now I copy. Later, we make lazy copy.
        new_state.models = [m for m in parent_state.models] # Creates a new list so we can add models without changing the original. Again, may want to do lazy.
        return new_state
"""
Assumes records are in rows, colums are fields
    row_range: (start_row: int, end_row: int)
    col_range: (start_col: int, n_cols: int)
"""
class Table:
    def __init__(self, filename, row_range: Tuple[int,int], col_range: Tuple[int,int]):
        self.filename = filename
        self.row_range = row_range
        self.col_range = col_range
        self.records = []
        self.fields = []

    def get_id_atom(self):
        from os.path import basename, splitext
        return "%s__%d_%d__%d_%d"%(self.filename, self.row_range[0], self.row_range[1], self.col_range[0], self.col_range[1])
    # Easy creation from earlier 
    @staticmethod
    def from_table(src: 'Table') -> 'Table':
        new_table = Table(src.filename, src.row_range, src.col_range)
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
