from .action_handler import ActionHandler

from .test_protocol import TestProtocolTask
from .dump_storable import DumpStorableTask

from .detect_tables import DetectTablesTask
from .get_fields import GetFieldTypesTask
from .get_field_headers import GetFieldHeadersTask
from .get_incomplete_fields import GetIncompleteFieldsTask
from .load_spreadsheet import LoadSpreadsheetTask

from .tensor_from_spec import TensorFromSpecTask
from .fold_tensor import FoldTensorTask
from .learn_countor import LearnCountORTask
from .generate_countor import GenerateCountORTask


from .join_tables import JoinTablesTask

# DEPRECATED
# from .get_table_structure import GetTableStructureTask


AVAILABLE_TASKS = {
    TestProtocolTask.PREDICATE: TestProtocolTask,
    DumpStorableTask.PREDICATE: DumpStorableTask,

    DetectTablesTask.PREDICATE: DetectTablesTask,
    LoadSpreadsheetTask.PREDICATE: LoadSpreadsheetTask,
    GetFieldTypesTask.PREDICATE: GetFieldTypesTask,
    GetFieldHeadersTask.PREDICATE: GetFieldHeadersTask,
    GetIncompleteFieldsTask.PREDICATE: GetIncompleteFieldsTask,
    
    TensorFromSpecTask.PREDICATE: TensorFromSpecTask,
    FoldTensorTask.PREDICATE: FoldTensorTask,
    LearnCountORTask.PREDICATE: LearnCountORTask,
    GenerateCountORTask.PREDICATE: GenerateCountORTask,
    
    JoinTablesTask.PREDICATE: JoinTablesTask
    # GetTableStructureTask.PREDICATE: GetTableStructureTask, # Replaced by GetFieldHeadersTask
}