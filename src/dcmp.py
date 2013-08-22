# We need to compare recursive dicts
def dcmp(a,b, compared = []):
    if id(a) == id(b):
        return True
    if a.__class__ != dict or b.__class__ != dict:
        return a == b
    if a in compared or b in compared:
        return True
    if a.keys() != b.keys():
        return False
    for key,val in a.iteritems():
        if not dcmp(a[key],b[key], compared + [a,b]):
            return False
    return True
