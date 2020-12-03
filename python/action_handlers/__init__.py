from .action_handler import ActionHandler

from .test_protocol import TestProtocolTask

from .detect_tables import DetectTablesTask
from .load_spreadsheet import LoadSpreadsheetTask
from .get_fields import GetFieldTypesTask
from .get_table_structure import GetTableStructureTask
from .learn_countor import LearnCountORTask

AVAILABLE_TASKS = {
    TestProtocolTask.PREDICATE: TestProtocolTask,

    DetectTablesTask.PREDICATE: DetectTablesTask,
    LoadSpreadsheetTask.PREDICATE: LoadSpreadsheetTask,
    GetFieldTypesTask.PREDICATE: GetFieldTypesTask,
    GetTableStructureTask.PREDICATE: GetTableStructureTask,
    LearnCountORTask.PREDICATE: LearnCountORTask
}
