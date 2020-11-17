from typing import List, Tuple

from .action_handler import ActionHandler
from idb_state.state import State, Table

from pj_protocol import JSONCompound, JSONList

class DetectTablesTask(ActionHandler):
    PREDICATE = "detect_tables"
    
    def tableset_to_JSONList(self, table_set):
        return JSONList([ JSONCompound('table', [ self.action_request.action_compound.args[0] ,t.get_id_atom()]) for t in table_set])

    def handle(self):
        state = self.load_state()
        raw = []

        # TODO: Figure out the filename, filekey mess.
        filekey = self.action_request.action_compound.args[0]
        with open(state.filename) as f: # MOCK: Clearly, the backend can't be hitting the file system
            for l in f:
                raw.append( [x.strip() for x in l.strip().split(',') if l.strip()] )
        
        connected_components = DetectTablesTask.detect_tables_cc(raw)
        table_sets = self.tables_from_cc(filekey, connected_components)
        result_list = []
        
        ar_args = self.action_request.action_compound.args
        for ts in table_sets:
            new_state = State.clone_parent(state)
            new_state.tables = ts
            self.state_manager.save_state(new_state)
            result = JSONCompound(DetectTablesTask.PREDICATE, [ar_args[0], self.tableset_to_JSONList(ts), ar_args[2]])
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
        print("\nNODES:\n" + str(nodes) + "\n\n")
        print("\EDGES:\n" + str(edges) + "\n\n")
        
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

    def tables_from_cc(self, filekey: str, components: List['DetectTablesTask.ConnectedComponent']):
        # TODO: Merging & Dividing tables based on density?
        return [
            [ Table(filekey, (0, 100), (0, 100)) ], # Hack to demonstrate multiple results:  
            [ Table(filekey, (cc.rmin, cc.rmax-cc.rmin+1), (cc.cmin, cc.cmax-cc.cmin+1)) for cc in components ]
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
        
