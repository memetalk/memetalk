from mmpprint import P
from collections import Iterable
from pdb import set_trace as br
import dshared

class Dict(dict):
    pass

def obj_eq(a,b):
    if id(a) == id(b):
        return True

    if isinstance(a, Iterable) and isinstance(b, Iterable):
        if '@id' in a and '@id' in b:
            return a['@id'] == b['@id']

        if isinstance(a, dshared.list):
            a = list(a)
        if isinstance(b, dshared.list):
            b = list(b)
    return a == b

# ID eq
def id_eq(a,b):
    assert(isinstance(a, Iterable))
    assert(isinstance(b, Iterable))

    if id(a) == id(b):
        return True
    if '@id' in a and '@id' in b:
        return a['@id'] == b['@id']
    return a == None and b == None
