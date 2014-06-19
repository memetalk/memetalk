from pyparsers.parser import MemeParser
from pyparsers.coretr import CoreTr
from pyparsers.astbuilder import *
from pyutils import bits
from . import vmem
from . import utils
import math
import struct
import ctypes
from pprint import pprint as P
from pdb import set_trace as br
import os


class Entry(object):
    def fill(self, c):
        raise Exception("Implement me")

class ObjectEntry(Entry):
    def __init__(self, name):
        self.name = name
        self.label = name
        self.slots = []

    def add_slot_ref(self, name, value):
        self.slots.append({'type': 'ref', 'name': name, 'value': value})

    def add_slot_literal_string(self, name, value):
        self.slots.append({'type': 'string', 'name': name, 'value': value})

    def add_slot_empty_dict(self, name):
        # TODO: this will become dict
        self.slots.append({'type': 'empty_dict', 'name': name})

    def add_slot_empty_list(self, name):
        # TODO: this will become list
        self.slots.append({'type': 'empty_list', 'name': name})

    def add_slot_literal_null(self, name):
        self.slots.append({'type': 'null', 'name': name})

    def add_slot_literal_num(self, name, value):
        self.slots.append({'type': 'int', 'name': name, 'value': value})

    def fill(self, c):
        # synth necessary objects:
        refs_to_literals = {}
        for idx, slot in enumerate(self.slots[1:]):
            if slot['type'] == 'string':
                refs_to_literals[idx] = append_string_instance(c, slot['value'])
            elif slot['type'] == 'empty_list':
                refs_to_literals[idx] = append_empty_list(c)
            elif slot['type'] == 'empty_dict':
                refs_to_literals[idx] = append_empty_dict(c)

        # emit our object

        oop = c.vmem.append_label_ref(self.slots[0]['value'], self.name) # first slot labeled by our name

        for idx, slot in enumerate(self.slots[1:]):
            if slot['type'] == 'ref':
                c.vmem.append_label_ref(slot['value'])
            elif slot['type'] == 'null':
                c.vmem.append_null()
            elif slot['type'] == 'string':
                c.vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'empty_list':
                c.vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'empty_dict':
                c.vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'int':
                c.vmem.append_int(slot['value'])
            else:
                raise Exception('TODO')
        return oop

class ClassEntry(Entry):
    def __init__(self, name, super_class, behavior_entry, cclass_entry):
        self.name = name
        self.label = utils.class_label(name)
        self.super_class = super_class
        self.behavior_entry = behavior_entry
        self.cclass_entry = cclass_entry
        self.dictionary = {}
        if super_class != 'Object':
            raise Exception('TODO')

    def compiled_class(self):
        return self.cclass_entry

    def set_fields(self, fields):
        self.cclass_entry.set_fields(fields)

    def add_method(self, fun):
        self.dictionary[fun.name] = fun

    def fill(self, c):
        oop_dict = append_dict_emiting_entries(c, self.dictionary)
        delegate = append_object_instance(c)

        oop = c.vmem.append_label_ref(utils.behavior_label(self.name),  self.label)  # vt
        c.vmem.append_pointer_to(delegate)                                           # delegate
        c.vmem.append_label_ref(self.super_class)                                    # parent
        c.vmem.append_pointer_to(oop_dict)                                           # dict: "methods"
        c.vmem.append_label_ref(utils.cclass_label(self.name))                # compiled_class
        return oop

class BehaviorEntry(Entry):
    def __init__(self, name, parent_name, dictionary):
        self.name = name
        self.label = utils.behavior_label(name)
        self.parent_name = parent_name
        self.parent_label = utils.behavior_label(parent_name)
        self.dictionary = dictionary
        if parent_name != 'Object':
            raise Exception('TODO')

    def fill(self, c):
        delegate = append_object_instance(c)
        oop_dict = append_empty_dict(c)
        oop = c.vmem.append_label_ref('Behavior', self.label)       # vt
        c.vmem.append_pointer_to(delegate)                          # delegate
        c.vmem.append_label_ref(self.parent_label)                  # parent
        c.vmem.append_pointer_to(oop_dict)                          # dict: "own methods"
        return oop


class CompiledClassEntry(Entry):
    def __init__(self, name, super_name):
        self.name = name
        self.label = utils.cclass_label(name)
        self.class_name = name
        self.super_name = super_name
        self.fields = []
        self.methods = {}

    def set_fields(self, fields):
        self.fields = fields

    def add_method(self, name, cfun):
        self.methods[name] = cfun

    def fill(self, c):
        delegate = append_object_instance(c)
        fields_list_oop = append_list_of_strings(c, self.fields)
        own_methods_oop = append_empty_dict(c)
        methods_oop = append_dict_emiting_entries(c, self.methods)
        name_oop = append_string_instance(c, self.class_name)
        super_name_oop = append_string_instance(c, self.super_name)

        oop = c.vmem.append_label_ref(utils.class_label('CompiledClass'), self.label)             # vt
        c.vmem.append_pointer_to(delegate)                                     # delegate
        c.vmem.append_pointer_to(name_oop)                                     # name
        c.vmem.append_pointer_to(super_name_oop)                               # super_class_name
        c.vmem.append_label_ref('@core_compiled_module')                       # compiled_module
        c.vmem.append_pointer_to(fields_list_oop)                              # fields
        c.vmem.append_pointer_to(methods_oop)                                  # methods: TODO
        c.vmem.append_pointer_to(own_methods_oop)                              # own methods: TODO
        return oop

class CompiledFunction(Entry):
    def __init__(self, owner_name, name, params, prim_name):
        self.owner_name = owner_name
        self.name = name
        self.label = utils.cfun_label(owner_name, name)
        self.params = params
        self.prim_name = prim_name

    def fill(self, c):
        delegate = append_object_instance(c)
        name_oop = append_string_instance(c, self.name)
        params_list_oop = append_list_of_strings(c, self.params)
        prim_name_oop = append_string_instance(c, self.prim_name)

        label = utils.cfun_label(self.owner_name, self.name)

        oop = c.vmem.append_label_ref(utils.class_label('CompiledFunction'), self.label)  # vt
        c.vmem.append_pointer_to(delegate)                          # delegate
        c.vmem.append_pointer_to(name_oop)                          # name
        c.vmem.append_pointer_to(params_list_oop)                   # params
        c.vmem.append_pointer_to(prim_name_oop)                     # prim_name
        c.vmem.append_label_ref(self.owner_name)                    # owner [CompiledClass/CompiledModule]
        return oop

class Function(Entry):
    def __init__(self, cfun, name):
        self.cfun = cfun
        self.name = name
        self.label = utils.fun_label(self.cfun.owner_name, self.name)

    def fill(self, c):
        delegate = append_object_instance(c)
        oop = c.vmem.append_label_ref('Function', self.label)     # vt
        c.vmem.append_pointer_to(delegate)                        # delegate
        c.vmem.append_label_ref(self.cfun.label)                  # compiled_function
        c.vmem.append_label_ref('@core_module')          # module
        return oop

class CoreCompiledModule(Entry):
    # core's CompiledModule instance
    def __init__(self):
        self.label = '@core_compiled_module'

    def compiled_functions_dict(self, c):
        # this assumes the cfun were not emited (ie, they are not part of c.entries)
        # -this is the place that emits module cfuns!

        cfun_entries = dict([(name, fun.cfun) for name, fun in c.top_level_functions.iteritems()])
        return append_dict_emiting_entries(c, cfun_entries)

        # for fun_entry in c.top_level_functions:
        #     keys.append(append_string_instance(c, fun_entry.name))

        # dict_oop = _append_dict_prologue(c, len(c.top_level_functions))
        # for idx, fun_entry in enumerate(c.top_level_functions):
        #     c.vmem.append_pointer_to(keys[idx])
        #     c.vmem.append_label_ref(fun_entry.cfun.label)
        # return dict_oop

    def compiled_classes_dict(self, c):
        # shit, this is complex. We can't emit the classes again so...
        # -first, emit string instances for the dict keys
        # -then, assembly the dict object and its pairs manually

        keys = []
        for entry in c.top_level_classes:
            keys.append(append_string_instance(c, entry.label))

        dict_oop = _append_dict_prologue(c, len(c.top_level_classes))
        for idx, entry in enumerate(c.top_level_classes):
            c.vmem.append_pointer_to(keys[idx])
            c.vmem.append_label_ref(entry.label)
        return dict_oop

    def fill(self, c):
        # vt, delegate, name, license, params
        # compiled_functions, compiled_classes
        # parent_module: <CompiledModule>

        delegate = append_object_instance(c)
        name_oop = append_string_instance(c, 'core')
        license_oop = append_string_instance(c, '')
        cfuns_dict = self.compiled_functions_dict(c)
        cclass_dict = self.compiled_classes_dict(c)

        oop = c.vmem.append_label_ref('CompiledModule', self.label)      # vt
        c.vmem.append_pointer_to(delegate)                               # delegate
        c.vmem.append_pointer_to(name_oop)                               # name
        c.vmem.append_pointer_to(license_oop)                            # license
        c.vmem.append_null()                                             # params
        c.vmem.append_pointer_to(cfuns_dict)                             # compiled_functions
        c.vmem.append_pointer_to(cclass_dict)                            # compiled_classes
        c.vmem.append_null()                                             # parent_module
        return oop

class CoreModule(Entry):
    # core's Module instance (conflated approach)
    def __init__(self):
        self.label = '@core_module'

    def create_dict(self, c):
        # dict

        # -Functions of the module
        return append_dict_emiting_entries(c, c.top_level_functions)
        pairs = []
        for fun_entry in c.top_level_functions.values():
            oop_key = append_string_instance(c, fun_entry.name)
            oop_val = fun_entry.fill(c)
            pairs.append((oop_key, oop_val))

        # -acessors for compiled classes
        for cclass in self.top_level_classes:
            cfun = CompiledFunction('@core_compiled_module', cclass.name, [], 'prim_get_' + cclass.name)
            cfun.fill(c)

            fun = Function(cfun, cclass.name)
            oop_key = append_string_instance(c, cclass.name)
            oop_val = fun.fill(c)
            pairs.append((oop_key, oop_val))

        return append_dict_with_pairs(c, pairs)


        # pairs = []
        # for fun in c.top_level_functions:
        #     name_key = append_string_instance(c, fun.name)
        #     oop_val = c.vmem.append_label_ref(fun.label)
        #     pairs.append((name_key, oop_val))

        # for entry in c.top_level_classes:
        #     name_key = append_string_instance(c, entry.name)
        #     cfun = CompiledFunction('@core_compiled_module', entry.name, [], 'prim_get_' + entry.name)
        #     cfun_oop = cfun.fill(c)

        #     fun = Function(cfun, entry.name, [])
        #     fun_oop = fun.fill(c)
        #     pairs.append((name_key, fun_oop))

        return append_dict_with_pairs(c, pairs)

    def fill(self, c):
        # vt, delegate, parent, dict, compiled_module, [entries]
        oop_dict = self.create_dict(c)
        oop = c.vmem.append_label_ref(self.label, self.label)     # vt
        c.vmem.append_null()                                      # delegate
        c.vmem.append_null()                                      # parent
        c.vmem.append_pointer_to(oop_dict)                        # dict
        c.vmem.append_label_ref('@core_compiled_module')          # compiled_module
        # for name in c.top_levels:
        #     c.vmem.append_label_ref(name)                         # compiled_module
        return oop

## instance entries


def append_object_instance(c):
    oop = c.vmem.append_label_ref('Object') # vt
    c.vmem.append_null()                    # delegate: end of chain of delegation
    return oop


def append_string_instance(c, string):
    if string in c.string_table:
        return c.string_table[string]
    else:
        delegate = append_object_instance(c) # Assumed to be object! if source change, this breaks
        oop = c.vmem.append_label_ref(utils.class_label('String'))   # vt
        c.vmem.append_pointer_to(delegate)        # delegate
        c.vmem.append_int(len(string))
        c.vmem.append_string(string)
        c.string_table[string] = oop
        return oop


## dict
def _append_dict_prologue(c, size):
    delegate = append_object_instance(c)        # Assumed to be object! if source change, this breaks
    oop = c.vmem.append_label_ref(utils.class_label('Dictionary')) # vt
    c.vmem.append_pointer_to(delegate)          # delegate
    c.vmem.append_int(size)                     # dict length
    return oop

def _append_dict_pairs(c, pairs):
    for key, val in pairs:
        c.vmem.append_pointer_to(key)
        c.vmem.append_pointer_to(val)

def append_empty_dict(c):
    return _append_dict_prologue(c, 0)

def append_dict_emiting_entries(c, entries_pydict):
    pairs_oop = []
    for key, entry, in entries_pydict.iteritems():
        key_oop = append_string_instance(c, key)
        val_oop = entry.fill(c)
        pairs_oop.append((key_oop, val_oop))

    return append_dict_with_pairs(c, pairs_oop)

def append_dict_with_pairs(c, pairs):
    oop = _append_dict_prologue(c, len(pairs))
    _append_dict_pairs(c, pairs)
    return oop

###

def append_empty_list(c):
    delegate = append_object_instance(c)   # Assumed to be object! if source change, this breaks
    oop = c.vmem.append_label_ref(utils.class_label('List'))       # vt
    c.vmem.append_pointer_to(delegate)          # delegate
    c.vmem.append_int(0)                        # len
    return oop


# used internally to create class fields list, etc.
def append_list_of_strings(c, lst):
    oops_elements = [append_string_instance(c, string) for string in lst]
    delegate = append_object_instance(c)   # Assumed to be object! if source change, this breaks
    oop = c.vmem.append_label_ref('List')       # vt
    c.vmem.append_pointer_to(delegate)          # delegate
    c.vmem.append_int(len(lst))                 # len
    for oop in oops_elements:                   # .. elements
        c.vmem.append_pointer_to(oop)
    return oop


class Compiler(ASTBuilder):
    def __init__(self):
        self.prime_names = []
        self.top_level_functions = {}
        self.top_level_classes = []
        self.top_levels = []

        self.entries = []
        self.current_object = None
        self.string_table = {}

        self.vmem = vmem.VirtualMemory()
        self.HEADER_SIZE = 3 * bits.WSIZE # bytes. 3 = names_size, entries, addr_table_offset

    def compile(self):
        self.line_offset = 0
        self.parser = MemeParser(open(os.path.join(os.path.dirname(__file__), '../mm/core.md'), 'r').read())
        self.parser.i = self
        ast = self.parser.apply("start")[0]
        self.parser = CoreTr([ast])
        self.parser.i = self
        self.parser.apply('start')


        self.synth_core_module()
        core = self.build_core()
        self.dump(core)

    ######################

    def register_object(self, name):
        self.top_levels.append(name)

        self.prime_names.append(name)
        self.current_object = ObjectEntry(name)
        self.entries.append(self.current_object)
        # there is no "class Object", so...
        if name == 'Object':
            self.top_level_classes.append(self.current_object)

    def register_class(self, name, super_class):
        self.top_levels.append(name)
        self.prime_names.append(name)
        # registering the behavior for that class
        behavior = BehaviorEntry(name, super_class, None)
        self.entries.append(behavior)

        # registering the compiled class for that class
        cclass = CompiledClassEntry(name, super_class)
        self.entries.append(cclass)

        self.current_class = ClassEntry(name, super_class, behavior, cclass)
        self.entries.append(self.current_class)
        self.top_level_classes.append(self.current_class)

    def add_class_fields(self, fields):
        self.current_class.set_fields(fields)

    def add_slot_ref(self, name, value):
        self.current_object.add_slot_ref(name, value)

    def add_slot_literal_null(self, name):
        self.current_object.add_slot_literal_null(name)

    def add_slot_literal_num(self, name, value):
        self.current_object.add_slot_literal_num(name, value)

    def add_slot_literal_string(self, name, value):
        self.current_object.add_slot_literal_string(name, value)

    def add_slot_literal_array(self, name, value):
        if len(value) > 0:
            raise Exception('todo')
        self.current_object.add_slot_empty_list(name)

    def add_slot_literal_dict(self, name, value):
        if len(value) > 0:
            raise Exception('todo')
        self.current_object.add_slot_empty_dict(name)

    def add_class_method(self, name, params, body_ast):
        primitive_name =  body_ast[0][1][0]
        cfun = CompiledFunction(self.current_class.cclass_entry.label, name, params, primitive_name)

        self.current_class.add_method(Function(cfun, name))

        self.current_class.compiled_class().add_method(name, cfun)

    def add_module_function(self, name, params, body_ast):
        primitive_name =  body_ast[0][1][1]
        cfun = CompiledFunction('@core_compiled_module', name, params, primitive_name)
        fun = Function(cfun, name)
        self.top_level_functions[name] = fun

    #################################################

    def synth_core_module(self):
        self.entries.append(CoreCompiledModule())
        self.entries.append(CoreModule())

    def name_ptr_for_name(self, name, core):
        acc = 0
        for entry_name_t, bsize in core['names']:
            if entry_name_t[0:-1] == name:
                return self.HEADER_SIZE + acc
            acc += bsize
        raise Exception('entry {} not found in NAMES'.format(name))

    def build_core(self):
        core = {
            'header': {
                'entries': None,             # number of labeled objects (top-levels, behaviors, cclasses)
                'names_size': None,          # size in bytes of NAMES section
                'ot_size': None},            # size of OBJECT TABLE in bytes
            'names': [],
            'index': [],
            'object_table': [],
            'addr_table': []}

        # -- HEADER and NAMES section
        core['header']['entries'] = len(self.entries)

        # names :: [(string, alloc-size in bytes, self-ptr)]
        core['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in [e.label + "\0" for e in self.entries]]

        core['header']['names_size'] = sum([x[1] for x in core['names']])

        index_size = core['header']['entries'] * 2 * bits.WSIZE # *2: pair (name, entry), *4: bytes
        base = self.HEADER_SIZE + core['header']['names_size'] + index_size
        self.vmem.set_base(base)

        # - OBJECT TABLE
        for entry in self.entries:
            entry.fill(self)

        core['object_table'] = self.vmem.object_table()

        # - HEADER ot_size
        core['header']['ot_size'] = len(core['object_table'])

        core['addr_table'] = self.vmem.addr_table()

        # - INDEX section
        for name in [x.label for x in self.entries]:
            core['index'].append(self.name_ptr_for_name(name, core))  # ptr to string
            core['index'].append(self.vmem.index_for(name))           # ptr to object

        return core

    def dump(self, core):
        fp = open("core.img", "w")

        # header
        fp.write(struct.pack('I', core['header']['entries']))
        fp.write(struct.pack('I', core['header']['names_size']))
        fp.write(struct.pack('I', core['header']['ot_size']))

        # names
        for name, chunk_size in core['names']:
            text = name + ((chunk_size - len(name)) * '\0')
            fp.write(text)

        # index
        for ptr in core['index']:
            fp.write(struct.pack('I', ptr))

        # object table
        for v8 in core['object_table']:
            fp.write(struct.pack('B', v8))

        # addr table
        for v32 in core['addr_table']:
            fp.write(struct.pack('I', v32))

        fp.close()

Compiler().compile()
