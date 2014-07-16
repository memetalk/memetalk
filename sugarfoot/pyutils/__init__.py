class Flag(object):
    def __init__(self):
        self.x = None
    def __call__(self, *args):
        if len(args) == 0:
            return self.x
        else:
            self.x = args[0]

FRAME_TYPE_OBJECT = 900
FRAME_TYPE_BVAR_OBJECT = 901 # len in bytes
FRAME_TYPE_LVAR_OBJECT = 902 # len in words
FRAME_TYPE_DVAR_OBJECT = 903 # len in words * 2
FRAME_TYPE_LITERAL_FRAME  = 904
FRAME_TYPE_BYTECODE_FRAME  = 905
FRAME_TYPE_EXCEPTIONS_FRAME  = 906


SEP = '_'

def behavior_label(name):
    return name + SEP + 'Behavior'

def cclass_label(name):
    return name + SEP + 'CompiledClass'

def class_label(name):
    return name # + SEP + "Class"

def cfun_label(owner_name, name):
    return owner_name + SEP + name + SEP + 'CompiledFunction'

def fun_label(cfun_label, name):
    return cfun_label + SEP + name + SEP + "Function"

def cmod_label(name):
    return name + SEP + "CompiledModule"

def mod_label(cmod_name):
    return cmod_name + SEP + "Module"
