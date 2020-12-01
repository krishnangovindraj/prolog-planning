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
        Field.DataTypes.DT_CAT: "dt_categorical",
        Field.DataTypes.DT_ID: "dt_id"
    }

    class GetFieldsPredicate(ActionHandler.ActionPredicate):
        def __init__(self, table_id, field_list):
            self.table_id = table_id
            self.field_list = field_list

        def to_json_compound(self):
            return JSONCompound(GetFieldTypesTask.PREDICATE, [self.table_id, self.field_list])

    def handle(self):
        req = GetFieldTypesTask.GetFieldsPredicate(*self.action_request.action_compound.args)
        table = self.idb.get_table(req.table_id)
        
        if table.fields is None: 
            table.fields = self.get_field_types(table)
        
        all_field_types = table.fields
        ft_lists = [ [t for t in GetFieldTypesTask.field_types_to_predicates(req.table_id, fi, all_field_types[fi])] for fi in range(len(all_field_types))]
        flat_ft_list = list(iter_chain(*ft_lists))
        
        return [ GetFieldTypesTask.GetFieldsPredicate(req.table_id, JSONList(flat_ft_list)) ]
    
    @staticmethod
    def field_types_to_predicates(table_id, field_index, field_types):
        return [
            JSONCompound('field_type', [table_id, field_index, GetFieldTypesTask.FIELD_TYPE_TO_STR[ft]]) 
            for ft in GetFieldTypesTask.FIELD_TYPE_TO_STR.keys() if field_types & ft] # for ft in field_types ]

    @staticmethod
    def get_field_types_of_value(val):
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
        val_types = 0 # set()

        if is_float(val):
            val_types |= DTT.DT_REAL # .add(DTT.DT_REAL)

        if is_int(val):
            val_types |= DTT.DT_INT # .add(DTT.DT_INT)
            if int(val) == 0 or int(val) == 1:
                val_types |= DTT.DT_BOOL # .add(DTT.DT_BOOL)
        else:
            val_types |= DTT.DT_STR # .add(DTT.DT_STR)
            if val.lower() in GetFieldTypesTask.BOOL_SET:
                val_types |= DTT.DT_BOOL # .add(DTT.DT_BOOL)
        
        return val_types

    def get_field_types(self, table):
        DTT = Field.DataTypes

        n_rows = len(table.records)
        n_cols = 0 if len(table.records) == 0 else len(table.records[0])
        field_types = []
        for j in range(n_cols):
            dtc = {}
            val_set = set()
            total_vals = 0
            for i in range(n_rows):
                val = table.records[i][j].strip()
                if not val:
                    continue
                    # Blank, we ignore
                total_vals+=1
                
                val_types = self.get_field_types_of_value(val) 
                if DTT.DT_REAL & val_types:
                    dtc[DTT.DT_REAL] = dtc.get(DTT.DT_REAL, 0) + 1 
                    val_set.add(float(val))
                
                if DTT.DT_INT & val_types:
                    dtc[DTT.DT_INT] = dtc.get(DTT.DT_INT, 0) + 1
                    val_set.add(int(val))
                    
                if DTT.DT_STR & val_types: # Should i have an if STR here?
                    dtc[DTT.DT_STR] = dtc.get(DTT.DT_STR, 0) + 1
                
                if DTT.DT_BOOL & val_types:
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
                
            all_types = primary_type # set([primary_type])
            if primary_type == DTT.DT_INT or primary_type == DTT.DT_STR:
                cardinality_threshold = max(3, min(20, non_blank//3) )
                if unique_vals <= cardinality_threshold:
                    if primary_type == DTT.DT_STR:
                        all_types |= DTT.DT_CAT # all_types.add( DTT.DT_CAT )
            
            if primary_type == DTT.DT_INT or primary_type == DTT.DT_STR:
                if unique_vals >= 0.95 * non_blank: # Some tolerance for repeating headers or something
                    all_types |= DTT.DT_ID # all_types.add(DTT.DT_ID)
            
            field_types.append(all_types)

        return field_types
