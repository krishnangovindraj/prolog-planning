from itertools import chain as iter_chain
from idb_state.state import Table, Field

from .action_handler import ActionHandler
from pj_protocol import JSONCompound, JSONList

class GetIncompleteFieldsTask(ActionHandler):
    PREDICATE = 'get_incomplete_fields'


    class GetIncompleteFieldsPredicate(ActionHandler.ActionPredicate):
        def __init__(self, table_id, n_header_rows, incomplete_fields):
            self.table_id = table_id
            self.n_header_rows = n_header_rows
            self.incomplete_fields = incomplete_fields

        def to_json_compound(self):
            return JSONCompound(GetIncompleteFieldsTask.PREDICATE, [self.table_id, self.n_header_rows, self.incomplete_fields])

    def handle(self):
        req = GetIncompleteFieldsTask.GetIncompleteFieldsPredicate(*self.action_request.action_compound.args)
        table = self.idb.get_table(req.table_id)
        n_headers = int(req.n_header_rows)
        incomplete_fields = self.get_incomplete_fields(table, n_headers)
        
        ic_list = [JSONCompound('incomplete_field', [j, b]) for j,b in enumerate(incomplete_fields) if b > 0 ]
        return [ GetIncompleteFieldsTask.GetIncompleteFieldsPredicate(req.table_id, req.n_header_rows, JSONList(ic_list)) ]
    

    def get_incomplete_fields(self, table, n_headers):
        blanks = []

        for j in range(table.n_cols()):
            blanks.append(0)
            for i in range(n_headers, table.n_rows()):
                val = table.records[i][j].strip()
                if not val:
                    blanks[j]+=1
        
        incomplete_fields = [ b/(table.n_rows()-n_headers) for b in blanks] 
        return incomplete_fields
