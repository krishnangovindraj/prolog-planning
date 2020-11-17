from .action_handler import ActionHandler

class TestProtocolTask(ActionHandler):
    PREDICATE = "test_protocol"
    def handle(self):
        return [self.action_request.action_compound]
