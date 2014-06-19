from pdb import set_trace as br

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
