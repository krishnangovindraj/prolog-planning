
from itertools import chain as iter_chain
from idb_state.state import Table, Field

from .action_handler import ActionHandler
from .get_fields import GetFieldTypesTask

from pj_protocol import JSONActionRequest, JSONCompound, JSONList

class GetFieldHeadersTask(ActionHandler):
    PREDICATE = 'get_field_headers'

    class GetFieldHeadersPredicate(ActionHandler.ActionPredicate):
        def __init__(self, table_id, n_header_rows, field_header_list):
            self.table_id = table_id
            self.n_header_rows = n_header_rows
            self.field_header_list = field_header_list

        def to_json_compound(self):
            return JSONCompound(GetFieldHeadersTask.PREDICATE, [self.table_id, self.n_header_rows, self.field_header_list])


    def handle(self):
        req = GetFieldHeadersTask.GetFieldHeadersPredicate(*self.action_request.action_compound.args)
        table = self.idb.get_table(req.table_id)
        
        # Do some work to detect header rows
        header_rows = self.detect_header_rows(table)
        table.header_rows = header_rows
        # Extract the values of each of the header rows into a list.
        n_fields = max([0] + [len(r) for r in table.records])        
        field_headers = [ [table.records[h][j]  for h in header_rows] for j in range(n_fields) ]
        
        n_header_rows = len(table.header_rows)

        # If there are no headers, don't bother with field titles?
        if len(header_rows) > 0:
            field_header_preds = [ JSONCompound('field_header', [table.get_id(), j, field_headers[j]] )  for j in range(n_fields) ]
        else:
            field_header_preds = []
        
        return [ GetFieldHeadersTask.GetFieldHeadersPredicate(req.table_id, n_header_rows, JSONList(field_header_preds)) ]
    

    def detect_header_rows(self, table):
        # Mockup: If all records are strings, it is a header
        DTT = Field.DataTypes
        nonstr_types = DTT.DT_REAL | DTT.DT_INT | DTT.DT_BOOL
        header_rows = []
        for i, r in enumerate(table.records):
            is_all_string = True
            for raw_val in r:
                val = raw_val.strip()
                if val and GetFieldTypesTask.get_field_types_of_value(val) & nonstr_types:
                        is_all_string = False
            
            if is_all_string:
                header_rows.append(i)
            else:
                break
                
        return header_rows
