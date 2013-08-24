from mmpprint import P

def obj_eq(a,b):
    if id(a) == id(b):
        return True
    if a.__class__ == dict and b.__class == dict:
        if '@id' in a and '@id' in b:
            return a['@id'] == b['@id']
    return a == b

# ID eq
def id_eq(a,b):
    if a.__class__ != dict:
        P(a)

    assert a.__class__ == dict, "'a' should be dict"
    assert b.__class__ == dict, "'b' should be dict"

    if id(a) == id(b):
        return True
    if '@id' in a and '@id' in b:
        return a['@id'] == b['@id']
    return a == None and b == None
