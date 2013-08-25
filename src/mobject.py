from mmpprint import P

class Dict(dict):
    pass

def obj_eq(a,b):
    if id(a) == id(b):
        return True
    if isinstance(a, dict) and isinstance(b, dict):
        if '@id' in a and '@id' in b:
            return a['@id'] == b['@id']
    return a == b

# ID eq
def id_eq(a,b):
    if a.__class__ != Dict:
        P(a)

    assert a.__class__ == Dict, "'a' should be dict"
    assert b.__class__ == Dict, "'b' should be dict"

    if id(a) == id(b):
        return True
    if '@id' in a and '@id' in b:
        return a['@id'] == b['@id']
    return a == None and b == None
