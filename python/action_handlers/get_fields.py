from itertools import chain as iter_chain
from idb_state.state import Table, Field

from .action_handler import ActionHandler
from pj_protocol import JSONCompound, JSONList

class GetFieldTypesTask(ActionHandler):
    PREDICATE = 'get_field_types'

    
    BOOL_SET = {"t", "true", "f", "false"}
    FIELD_TYPE_TO_STR = {
        Field.DataTypes.DT_INT: "dt_int",
        Field.DataTypes.DT_REAL: "dt_float",
        Field.DataTypes.DT_STR: "dt_string",
        Field.DataTypes.DT_BOOL: "dt_bool",
        Field.DataTypes.DT_CAT: "dt_categorical"
    }

    class GetFieldsPredicate(ActionHandler.ActionPredicate):
        def __init__(self, table_id, field_list):
            self.table_id = table_id
            self.field_list = field_list

        def to_json_compound(self):
            return JSONCompound(GetFieldTypesTask.PREDICATE, [self.table_id, self.field_list])

    @staticmethod
    def field_types_to_predicates(field_index, field_types):
        return [
            JSONCompound('field_type', [field_index, GetFieldTypesTask.FIELD_TYPE_TO_STR[ft]]) 
            for ft in field_types]


    def handle(self):
        req = GetFieldTypesTask.GetFieldsPredicate(*self.action_request.action_compound.args)
        table = self.idb.get_table(req.table_id)
        all_field_types = self.get_field_types(table)
        ft_lists = [ [t for t in GetFieldTypesTask.field_types_to_predicates(fi, all_field_types[fi])] for fi in range(len(all_field_types))]
        flat_ft_list = list(iter_chain(*ft_lists))
        return [ GetFieldTypesTask.GetFieldsPredicate(req.table_id, JSONList(flat_ft_list)) ]
    
    def get_field_types(self, table):
        def is_int(value):
            try:
                int(value)
                return True
            except ValueError:
                return False

        def is_float(value):
            try:
                float(value)
                return True
            except ValueError:
                return False

        DTT = Field.DataTypes

        n_rows = len(table.records)
        n_cols = 0 if len(table.records) == 0 else len(table.records[0])
        field_types = []
        for j in range(n_cols):
            dtc = {}
            val_set = set()
            total_vals = 0
            for i in range(n_rows):
                val = table.records[i][j]
                if not val.strip():
                    continue
                    # Blank, we ignore
                total_vals+=1
                
                if is_float(val):
                    dtc[DTT.DT_REAL] = dtc.get(DTT.DT_REAL, 0) + 1 
                    val_set.add(float(val))
                
                if is_int(val):
                    dtc[DTT.DT_INT] = dtc.get(DTT.DT_INT, 0) + 1
                    val_set.add(int(val))
                    if int(val) == 0 or int(val) == 1:
                        dtc[DTT.DT_BOOL] = dtc.get(DTT.DT_BOOL, 0) + 1
                else:
                    dtc[DTT.DT_STR] = dtc.get(DTT.DT_STR, 0) + 1
                    if val.lower() in GetFieldTypesTask.BOOL_SET:
                        dtc[DTT.DT_BOOL] = dtc.get(DTT.DT_BOOL, 0) + 1
            
            # Now we have the count. The max wins 
            maxval = max(dtc.values())
            primary_type = None
            if dtc.get(DTT.DT_BOOL,0) >= maxval:
                primary_type = DTT.DT_BOOL
            elif dtc.get(DTT.DT_INT,0) >= maxval:
                primary_type = DTT.DT_INT
            elif dtc.get(DTT.DT_REAL,0) >= maxval:
                primary_type = DTT.DT_REAL
            else:
                primary_type = DTT.DT_STR

            # Some funky heuristic for determining if it can be categorical
            unique_vals = len(val_set)
            non_blank = total_vals - unique_vals
                
            all_types = set([primary_type])
            if primary_type == DTT.DT_INT or primary_type == DTT.DT_STR:
                cardinality_threshold = max(3, min(20, non_blank//3) )
                if unique_vals <= cardinality_threshold:
                    if primary_type == DTT.DT_STR:
                        all_types.add( DTT.DT_CAT )
            
            if primary_type == DTT.DT_INT or primary_type == DTT.DT_STR:
                if unique_vals >= 0.95 * non_blank: # Some tolerance for repeating headers or something
                    all_types.add(DTT.DT_ID)
            
            field_types.append(all_types)

        return field_types