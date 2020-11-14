# from __future__ import annotations
from typing import List


class PrologJSONProtocolObject:
    def to_dict(self):
        raise NotImplementedError("Abstract")


class AbstractJSONTerm(PrologJSONProtocolObject):
    @staticmethod
    def from_dict(term_dict) -> 'AbstractJSONTerm':
        if isinstance(term_dict, dict) and term_dict["type"] == JSONCompound.PJ_TYPE:
            return JSONCompound.from_dict(term_dict)
        elif isinstance(term_dict, dict) and term_dict["type"] == JSONList.PJ_TYPE:
            return JSONList.from_dict(term_dict)
        else:
            return term_dict


class JSONCompound(AbstractJSONTerm):
    PJ_TYPE = "json_compound"
    def __init__(self, pred_name: str, args: List):
        self.pred_name = pred_name
        self.args = args

    def to_list_rep(self):
        return [self.pred_name] +  self.args

    def to_dict(self):
        return {"type": JSONCompound.PJ_TYPE, "list_rep": self.to_list_rep()}

    @staticmethod
    def from_dict(comp_dict) -> 'JSONCompound':
        assert(comp_dict["type"] == JSONCompound.PJ_TYPE)
        return JSONCompound( comp_dict["list_rep"][0], list(map(AbstractJSONTerm.from_dict, comp_dict["list_rep"][1:])) )

    def __str__(self):
        return "JSONCompound:%s(%s)"%(self.pred_name, ", ".join(map(str,self.args)))


class JSONList(PrologJSONProtocolObject):
    PJ_TYPE = "json_list"
    def __init__(self, elements: List):
        self.elements = elements

    def to_dict(self):
        return {"type": JSONList.PJ_TYPE, "elements": self.elements}

    @staticmethod
    def from_dict(jlist_dict) -> 'JSONList':
        assert(jlist_dict["type"] == JSONList.PJ_TYPE)
        return JSONList( list(map(AbstractJSONTerm.from_dict, jlist_dict["elements"])) )

    def __str__(self):
        return "JSONList:[%s]"%(", ".join(map(str,self.elements)))


class JSONResultList(PrologJSONProtocolObject):
    PJ_TYPE = "json_result_list"
    def __init__(self, results: List[JSONCompound]):
        self.results = results
    
    def to_dict(self):
        return {"type": JSONResultList.PJ_TYPE, "results": self.results}

    @staticmethod
    def from_dict(result_dict) -> 'JSONResultList':
        assert(result_dict["type"] == JSONResultList.PJ_TYPE)
        return JSONResultList(list(map, JSONCompound.from_dict(result_dict["results"])))
    
    def __str__(self):
        return "JSONResultList([%s])"%(", ".join(map(str,self.results)))


class JSONActionRequest(PrologJSONProtocolObject):
    PJ_TYPE = "json_action_request"
    def __init__(self, action_compound):
        self.action_compound = action_compound

    def to_dict(self):
        return {"type": JSONActionRequest.PJ_TYPE, "action": self.action_compound.to_dict()}

    @staticmethod
    def from_dict(action_dict) -> 'JSONActionRequest':
        assert(action_dict["type"]==JSONActionRequest.PJ_TYPE)
        return JSONActionRequest(JSONCompound.from_dict(action_dict["action"]))
    
    def __str__(self):
        return "JSONActionRequest(%s)"%( str(self.action_compound) )
