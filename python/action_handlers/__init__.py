from .action_handler import ActionHandler

from .test_protocol import TestProtocolTask

from .detect_tables import DetectTablesTask
from .get_fields import GetFieldTypesTask
from .get_field_headers import GetFieldHeadersTask
from .get_incomplete_fields import GetIncompleteFieldsTask
from .load_spreadsheet import LoadSpreadsheetTask

from .tensor_from_spec import TensorFromSpecTask
from .learn_countor import LearnCountORTask
from .join_tables import JoinTablesTask

from .get_table_structure import GetTableStructureTask


AVAILABLE_TASKS = {
    TestProtocolTask.PREDICATE: TestProtocolTask,

    DetectTablesTask.PREDICATE: DetectTablesTask,
    LoadSpreadsheetTask.PREDICATE: LoadSpreadsheetTask,
    GetFieldTypesTask.PREDICATE: GetFieldTypesTask,
    GetFieldHeadersTask.PREDICATE: GetFieldHeadersTask,
    GetIncompleteFieldsTask.PREDICATE: GetIncompleteFieldsTask,
    
    TensorFromSpecTask.PREDICATE: TensorFromSpecTask,
    LearnCountORTask.PREDICATE: LearnCountORTask,
    
    JoinTablesTask.PREDICATE: JoinTablesTask
    # GetTableStructureTask.PREDICATE: GetTableStructureTask, # Replaced by GetFieldHeadersTask
}
