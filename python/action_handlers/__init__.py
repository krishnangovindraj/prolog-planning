from .action_handler import ActionHandler

from .test_protocol import TestProtocolTask

from .detect_tables import DetectTablesTask
from .load_spreadsheet import LoadSpreadsheetTask
from .get_fields import GetFieldTypesTask
AVAILABLE_TASKS = {
    TestProtocolTask.PREDICATE: TestProtocolTask,

    DetectTablesTask.PREDICATE: DetectTablesTask,
    LoadSpreadsheetTask.PREDICATE: LoadSpreadsheetTask,
    GetFieldTypesTask.PREDICATE: GetFieldTypesTask
}