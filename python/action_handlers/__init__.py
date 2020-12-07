from .action_handler import ActionHandler

from .test_protocol import TestProtocolTask

from .detect_tables import DetectTablesTask
from .get_fields import GetFieldTypesTask
from .get_field_headers import GetFieldHeadersTask
from .load_spreadsheet import LoadSpreadsheetTask
from .learn_countor import LearnCountORTask

# from .get_table_structure import GetTableStructureTask


AVAILABLE_TASKS = {
    TestProtocolTask.PREDICATE: TestProtocolTask,

    DetectTablesTask.PREDICATE: DetectTablesTask,
    LoadSpreadsheetTask.PREDICATE: LoadSpreadsheetTask,
    GetFieldTypesTask.PREDICATE: GetFieldTypesTask,
    GetFieldHeadersTask.PREDICATE: GetFieldHeadersTask,
    LearnCountORTask.PREDICATE: LearnCountORTask
    
    # GetTableStructureTask.PREDICATE: GetTableStructureTask, # Replaced by GetFieldHeadersTask
}
