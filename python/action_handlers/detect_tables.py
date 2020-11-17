from typing import List, Tuple

from .action_handler import ActionHandler
from idb_state.state import SpreadSheet, Table, CellRange

from pj_protocol import JSONCompound, JSONList

class DetectTablesTask(ActionHandler):
    PREDICATE = "detect_tables"
    # TODO:Request objects so I can change the signature without breaking too much.
    class DetectTablesPredicate(ActionHandler.ActionPredicate):
        def __init__(self, spreadsheet_id, table_list):
            self.spreadsheet_id = spreadsheet_id
            self.table_list = table_list

        def to_json_compound(self):
            return JSONCompound(DetectTablesTask.PREDICATE, [self.spreadsheet_id, self.table_list])

    def tableset_to_JSONList(self, ss_id, table_set):
        # Warning. Tables must already be stored.
        return JSONList([ JSONCompound('table', [ ss_id, t.get_id()]) for t in table_set ])


    def handle(self):
        dt_pred = DetectTablesTask.DetectTablesPredicate(*self.action_request.action_compound.args)
        ss_id = dt_pred.spreadsheet_id

        ss = self.idb.get_spreadsheet(ss_id)
        raw = []

        # TODO: Figure out the filename, filekey mess.
        with open(ss.filename) as f: # MOCK: Clearly, the backend can't be hitting the file system
            for l in f:
                if l.strip():
                    raw.append( [x.strip() for x in l.strip().split(',')] )
        
        connected_components = DetectTablesTask.detect_tables_cc(raw)
        table_sets = self.tables_from_cc(ss, connected_components)
        result_list = []
        
        for ts in table_sets:
            # new_ss = SpreadSheet(ss.filename, ss)
            # new_ss.tables = ts
            # self.idb.add_spreadsheet(new_ss)
            for t in ts:
                t.records = [raw[r][t.origin.col_start:t.origin.col_start+t.origin.n_cols] 
                                for r in range(t.origin.row_start, t.origin.row_start+t.origin.n_rows)]
                self.idb.add_table(t)
                # print((t._id, t._in_db, t.get_id()) )
            
            result = DetectTablesTask.DetectTablesPredicate(ss.get_id(), self.tableset_to_JSONList(ss.get_id(), ts))
            result_list.append( result )
        return result_list

    # Actually, let's do a graph of connected components [and then try cuts.]
    @staticmethod
    def detect_tables_cc(raw):
        nodes = set()
        edges = dict()
        n_rows = len(raw)
        n_cols = max([len(r) for r in raw])    
        for i in range(n_rows):
            for j in range(n_cols):
                if raw[i][j]:
                    nodes.add((i,j))
                    edges[(i,j)] = []

        # Now connected components
        for (i,j) in nodes:
            if i > 0 and (i-1,j) in nodes:
                edges[(i,j)].append( (i-1, j) )
            if i < n_rows-1 and (i+1,j) in nodes:
                edges[(i,j)].append( (i+1, j) )
            if j > 0 and (i,j-1) in nodes:
                edges[(i,j)].append( (i, j-1) )
            if j < n_cols-1 and (i,j+1) in nodes:
                edges[(i,j)].append( (i, j+1) )

        seen = set()
        connected_components = []
        # print("\nNODES:\n" + str(nodes) + "\n\n")
        # print("\EDGES:\n" + str(edges) + "\n\n")
        
        for n in nodes:
            if n in seen:
                continue
            
            i,j = n
            stack = [n]
            cc = DetectTablesTask.ConnectedComponent()
            while len(stack) > 0:
                v = stack.pop()
                cc.add(v)

                for vv in edges[v]:
                    if vv not in seen:
                        stack.append(vv)
                        seen.add(vv)
                
            connected_components.append(cc)
        return connected_components

    def tables_from_cc(self, origin_spreadsheet: SpreadSheet, components: List['DetectTablesTask.ConnectedComponent']):
        # TODO: Merging & Dividing tables based on density?
        return [
            # [ Table(CellRange(origin_spreadsheet.get_id(), 0, 0, 100, 100), None) ], # Hack to demonstrate multiple results:  
            [ Table(CellRange(origin_spreadsheet.get_id(), cc.rmin, cc.cmin, cc.rmax-cc.rmin+1, cc.cmax-cc.cmin+1), None) for cc in components ]
        ]

    class ConnectedComponent:
        def __init__(self):
            self.nodes = []
            self.rmin = 112345678
            self.rmax = -1
            self.cmin = 112345678
            self.cmax = -1
            # TODO: row/col density dicts

        def add(self, v):
            self.nodes.append(v)
            self.rmin = min(v[0], self.rmin)
            self.rmax = max(v[0], self.rmax)
            self.cmin = min(v[1], self.cmin)
            self.cmax = max(v[1], self.cmax)
        
