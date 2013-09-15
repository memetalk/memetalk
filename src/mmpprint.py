import pprint
def my_safe_repr(object, context, maxlevels, level):
    typ = pprint._type(object)
    if typ is unicode:
        object = str(object)
    return pprint._safe_repr(object, context, maxlevels, level)

def P(obj, depth=1, ret = False):
    if depth == -1:
        depth = None
    printer = pprint.PrettyPrinter(1,80,depth)
    printer.format = my_safe_repr
    if ret:
        return printer.pformat(obj)
    else:
        print printer.pformat(obj)
