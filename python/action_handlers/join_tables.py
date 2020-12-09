
from itertools import chain as iter_chain
from idb_state.state import Table, Field

from .action_handler import ActionHandler
from .get_fields import GetFieldTypesTask

from pj_protocol import JSONActionRequest, JSONCompound, JSONList

class JoinTablesTask(ActionHandler):
    PREDICATE = 'join_tables'

    class JoinTablesPredicate(ActionHandler.ActionPredicate):
        def __init__(self, t1_id, t2_id, join_spec, t3_ss_id, t3_id, t3_rows, t3_cols):
            self.t1_id = t1_id
            self.t2_id = t2_id
            self.join_spec = join_spec
            self.t3_ss_id = t3_ss_id
            self.t3_id = t3_id
            self.t3_rows = t3_rows
            self.t3_cols = t3_cols
            
        def to_json_compound(self):
            return JSONCompound(JoinTablesTask.PREDICATE, [self.t1_id, self.t2_id, self.join_spec,
                self.t3_ss_id, self.t3_id, self.t3_rows, self.t3_cols])


    def handle(self):
        req = JoinTablesTask.JoinTablesPredicate(*self.action_request.action_compound.args)
        t1 = self.idb.get_table(req.t1_id)
        t2 = self.idb.get_table(req.t2_id)
        jspec = tuple(req.join_spec.elements)
        
        t1_headers = [t1.records[i] for i in t1.header_rows]
        t1_fields = [ tuple([h[j] for h in t1_headers]) for j in range(t1.n_cols()) ]
        
        t2_headers = [t2.records[i] for i in t2.header_rows]
        t2_fields = [ tuple([h[j] for h in t2_headers]) for j in range(t2.n_cols()) ]

        try:
            i1 = t1_fields.index(jspec)
            i2 = t2_fields.index(jspec)
        except ValueError as e:
            raise(e) # Fail off

        # Create the index on t2
        t2_index = {}
        for r in t2.get_records(False):
            if r[i2] not in t2_index:
                t2_index[r[i2]] = []
                t2_index[r[i2]].append(r)

        # Prepare the headers.
        records = []
        records.append( (
            [t1.get_id()] * t1.n_cols() +
            [t2.get_id()] * t2.n_cols()
        ))

        from itertools import zip_longest
        
        t1_header_rows = [t1.records[h] for h in t1.header_rows]
        t2_header_rows = [t2.records[h] for h in t2.header_rows]
        
        for h1,h2 in zip_longest(t1_header_rows, t2_header_rows):
            records.append(h1+h2)

        for r1 in t1.records:
            for r2 in t2_index.get( r1[i2], []):
                records.append(r1+r2)

        t3 = Table(None, None)
        t3.records = records
        self.idb.add_table(t3)


        return  [ JoinTablesTask.JoinTablesPredicate(
                    t1.get_id(), t2.get_id(), req.join_spec,
                    JSONCompound('joined', [t1.get_id(), t2.get_id()]),
                        t3.get_id(), t3.n_rows(), t3.n_cols()
                )]
    

