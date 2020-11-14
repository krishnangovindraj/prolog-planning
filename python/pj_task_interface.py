from typing import Dict

from pj_protocol import JSONActionRequest, JSONResultList

""" For now, this is a dummy which just returns a (result) list 
which contains only the requested action-predicate """
class PJTaskInterface:

    @staticmethod
    def process_request(request: Dict):
        action_request = JSONActionRequest.from_dict(request)
        print(action_request)
        result_list = [action_request.action_compound]

        return JSONResultList(result_list)
