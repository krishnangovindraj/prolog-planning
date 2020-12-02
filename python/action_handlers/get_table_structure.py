
from itertools import chain as iter_chain
from idb_state.state import Table, Field

from .action_handler import ActionHandler
from .get_fields import GetFieldTypesTask

from pj_protocol import JSONActionRequest, JSONCompound, JSONList

class GetTableStructureTask(ActionHandler):
    PREDICATE = 'get_table_structure'

    class GetTableStructurePredicate(ActionHandler.ActionPredicate):
        def __init__(self, table_id, table_structure):
            self.table_id = table_id
            self.table_structure = table_structure

        def to_json_compound(self):
            return JSONCompound(GetTableStructureTask.PREDICATE, [self.table_id, self.table_structure])


    def handle(self):
        req = GetTableStructureTask.GetTableStructurePredicate(*self.action_request.action_compound.args)
        table = self.idb.get_table(req.table_id)
        

        n_fields = max([0] + [len(r) for r in table.records])        
        # Do some work to detect header rows
        header_rows = self.detect_header_rows(table)
        # Extract the values of each of the header rows into a list.
        field_headers = [ [table.records[h][j]  for h in header_rows] for j in range(n_fields) ]
        # For data range
        data_start = max([-1] + header_rows) + 1
        
        
        # Turn these into predicates
        n_fields_pred = JSONCompound('table_n_fields', [table.get_id(), n_fields])
        data_range_pred = JSONCompound('table_data_range', [table.get_id(), data_start, len(table.records)])
        
        header_row_preds = [ JSONCompound('table_header_row', [table.get_id(), h ]) for h in header_rows ]
        
        # If there are no headers, don't bother with field titles?
        if len(header_rows) > 0:
            field_titles_preds = [ JSONCompound('table_field_title', [table.get_id(), j, field_headers[j]] )  for j in range(n_fields) ]
        else:
            field_titles_preds = []
        
        # Pack em all up
        structure = [n_fields_pred, data_range_pred] + header_row_preds + field_titles_preds 
        
        return [ GetTableStructureTask.GetTableStructurePredicate(req.table_id, s) for s in structure ]
    

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
