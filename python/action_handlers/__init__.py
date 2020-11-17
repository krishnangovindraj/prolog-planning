from .action_handler import ActionHandler

from .detect_tables import DetectTablesTask
from .load_spreadsheet import LoadSpreadsheetTask

AVAILABLE_TASKS = {
    DetectTablesTask.PREDICATE: DetectTablesTask,
    LoadSpreadsheetTask.PREDICATE: LoadSpreadsheetTask    
}