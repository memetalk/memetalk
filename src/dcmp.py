def dcmp(a,b):
    if id(a) == id(b):
        return True
    if a.__class__ == dict and b.__class__ == dict:
        if '@id' in a and '@id' in b:
            return a['@id'] == b['@id']
    return a == b
