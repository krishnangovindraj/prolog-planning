from typing import Dict

from pj_protocol import JSONActionRequest, JSONResultList

""" For now, this is a dummy which just a mockup of certain tasks.
Eventually we could try to build this around the existing synth-services back.
We'd also need a lot of extra endpoints to get enough info into prolog to reason.   
which contains only the requested action-predicate """
class PJTaskInterface:

    @staticmethod
    def process_request(request: Dict):
        action_request = JSONActionRequest.from_dict(request)
        print(action_request)
        result_list = [action_request.action_compound]

        return JSONResultList(result_list)

    @staticmethod
    def resolve(request: JSONActionRequest):
        if request.action_compound.predicate 
        pass