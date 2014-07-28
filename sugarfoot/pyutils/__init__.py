class OrderedDict(dict):
    def __init__(self):
        self.lst = []

    def __getitem__(self, key):
        p = [x for x in self.lst if x[0] == key]
        if len(p) == 0:
            raise KeyError(key)
        return p[0][1]

    def __setitem__(self, key, val):
        p = [x for x in self.lst if x[0] == key]
        if len(p) == 0:
            self.lst.append((key, val))
        else:
            p[0][1] = val

    def items(self):
        return self.lst

    def __iter__(self, *args):
        return iter(self.keys())

    def __contains__(self, key):
        return key in self.keys()

    def iteritems(self):
        return iter(self.items())

    def keys(self):
        return [x[0] for x in self.lst]

    def values(self):
        return [x[1] for x in self.lst]

    def __len__(self):
        return len(self.lst)

class Flag(object):
    def __init__(self):
        self.x = None
    def __call__(self, *args):
        if len(args) == 0:
            return self.x
        else:
            self.x = args[0]

FRAME_TYPE_OBJECT = 900
FRAME_TYPE_BVAR_OBJECT = 901 # len in bytes (symbols/strings)
FRAME_TYPE_LIST_OBJECT = 902
FRAME_TYPE_DVAR_OBJECT = 903 # len in words * 2
FRAME_TYPE_LITERAL_FRAME  = 904
FRAME_TYPE_BYTECODE_FRAME  = 905
FRAME_TYPE_EXCEPTIONS_FRAME  = 906
FRAME_TYPE_ELEMENTS = 907


SEP = '_'

def behavior_label(name):
    return name + SEP + 'Behavior'

def cclass_label(name):
    return name + SEP + 'CompiledClass'

def class_label(name):
    return name # + SEP + "Class"

_closures = 0
def closure_name():
    global _closures
    _closures += 1
    return '<anonymous ' + str(_closures) + '>'

_cfun_id = 0
def cfun_label(owner_label, name, is_method):
    global _cfun_id
    if is_method:
        # create unique label, so we can have an instance method with
        # the same name as a class method.
        _cfun_id += 1
        return str(_cfun_id) + "_" + owner_label + SEP + name + SEP + 'CompiledFunction'
    else:
        return owner_label + SEP + name + SEP + 'CompiledFunction'

def fun_label(cfun_label):
    return cfun_label + SEP + "Function"

def cmod_label(name):
    return name + SEP + "CompiledModule"

def mod_label(cmod_name):
    return cmod_name + SEP + "Module"
