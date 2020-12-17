from itertools import chain as iter_chain
from idb_state.state import Table, Field

from .action_handler import ActionHandler
from pj_protocol import JSONCompound, JSONList

from itertools import product

class GetTableContentsTask(ActionHandler):
    PREDICATE = 'get_table_contents'

    
    class GetTableContentsPredicate(ActionHandler.ActionPredicate):
        def __init__(self, table_id, cell_list):
            self.table_id = table_id
            self.cell_list = cell_list

        def to_json_compound(self):
            return JSONCompound(GetTableContentsTask.PREDICATE, 
                [self.table_id, self.cell_list])

    def handle(self):
        
        req = GetTableContentsTask.GetTableContentsPredicate(*self.action_request.action_compound.args)
        table = self.idb.get_table(req.table_id)
        
        cell_list = [JSONCompound('table_cell', [req.table_id, r, c, table.records[r][c]])
                         for r,c in product(range(table.n_rows()), range(table.n_cols()))]

        return [ GetTableContentsTask.GetTableContentsPredicate(req.table_id, JSONList(cell_list)) ]
    