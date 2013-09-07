import pickle as pickle
from cStringIO import StringIO
import ctypes
from mmpprint import P


# def get_object_by_id(idnum):
#     return ctypes.cast(idnum, ctypes.py_object).value

# def persistent_id(obj):
#     if hasattr(obj, '__module__') and 'PyQt' in obj.__module__:
#         return str(obj) + "::" + str(id(obj))
#     else:
#         return None

# def persistent_load(persid):
#     if type(persid) == str and 'PyQt' in persid:
#         objid =  long(persid[persid.index("::")+2:])
#         obj = get_object_by_id(objid)
#         return obj
#     else:
#         raise pickle.UnpicklingError, 'Invalid persistent id'

# def pickle_data(data):
#     src = StringIO()
#     p = pickle.Pickler(src)
#     p.persistent_id = persistent_id
#     print '+++pickling data'
#     try:
#         a_tank = Tank( data )
#         p.dump(a_tank)
#     except Exception as e:
#         print "+++ pickling data EXC: " + str(e)
#     print '+++pickling DONE data'
#     return src.getvalue()
#     #datastream = src.getvalue()
#     #print repr(datastream)

# def unpickle_data(data_str):
#     dst = StringIO(data_str)
#     up = pickle.Unpickler(dst)
#     up.persistent_load = persistent_load
#     print "++LOADING pickled data"
#     ret = up.load()
#     print "++LOADED pickled data"
#     return ret.lift()

def put(channel, obj):
    try:
        channel.put(obj)
    except Exception:
        P(obj, 5)
        raise

def get(channel):
    ret = channel.get()
    return ret
