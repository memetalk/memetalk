from parser import MemeParser
from coretr import CoreTr
from astbuilder import *
import vmem
import math
from pdb import set_trace as br
import struct
import ctypes
from . import utils
from pprint import pprint as P
import os


class Entry(object):
    def fill(self, vmem):
        raise Exception("Implement me")

    def get_name(self):
        return self.name

class ObjectEntry(Entry):
    def __init__(self, name):
        self.name = name
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

    def fill(self, vmem):
        # synth necessary objects:
        refs_to_literals = {}
        for idx, slot in enumerate(self.slots[1:]):
            if slot['type'] == 'string':
                refs_to_literals[idx] = append_string_instance(vmem, slot['value'])
            elif slot['type'] == 'empty_list':
                refs_to_literals[idx] = append_empty_list(vmem)
            elif slot['type'] == 'empty_dict':
                refs_to_literals[idx] = append_empty_dict(vmem)

        # emit our object

        oop = vmem.append_label_ref(self.slots[0]['value'], self.name) # first slot labeled by our name

        for idx, slot in enumerate(self.slots[1:]):
            if slot['type'] == 'ref':
                vmem.append_label_ref(slot['value'])
            elif slot['type'] == 'null':
                vmem.append_null()
            elif slot['type'] == 'string':
                vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'empty_list':
                vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'empty_dict':
                vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'int':
                vmem.append_int(slot['value'])
            else:
                raise Exception('TODO')
        return oop

class ClassEntry(Entry):
    def __init__(self, name, super_class, behavior_entry, cclass_entry):
        self.name = name
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

    def fill(self, vmem):
        oop_dict = append_entries_dict(vmem, self.dictionary)
        delegate = append_object_instance(vmem)

        oop = vmem.append_label_ref(utils.behavior_name(self.name), self.name)   # vt
        vmem.append_pointer_to(delegate)                                         # delegate
        vmem.append_label_ref(self.super_class)                                  # parent
        vmem.append_pointer_to(oop_dict)                                         # dict: "methods"
        vmem.append_label_ref(utils.compiled_class_name(self.name))              # compiled_class
        return oop

class BehaviorEntry(Entry):
    def __init__(self, name, parent_name, dictionary):
        self.name = utils.behavior_name(name)
        self.parent_name = utils.behavior_name(parent_name)
        self.dictionary = dictionary
        if parent_name != 'Object':
            raise Exception('TODO')

    def fill(self, vmem):
        delegate = append_object_instance(vmem)
        oop_dict = append_empty_dict(vmem)
        oop = vmem.append_label_ref('Behavior', self.name)        # vt
        vmem.append_pointer_to(delegate)                          # delegate
        vmem.append_label_ref(self.parent_name)                   # parent
        vmem.append_pointer_to(oop_dict)                          # dict: "own methods"
        return oop


class CompiledClassEntry(Entry):
    def __init__(self, name, super_name):
        self.name = utils.compiled_class_name(name)
        self.class_name = name
        self.super_name = super_name
        self.fields = []
        self.methods = {}

    def set_fields(self, fields):
        self.fields = fields

    def add_method(self, name, cfun):
        self.methods[name] = cfun

    def fill(self, vmem):
        delegate = append_object_instance(vmem)
        fields_list_oop = append_list_of_strings(vmem, self.fields)
        own_methods_oop = append_empty_dict(vmem)
        methods_oop = append_entries_dict(vmem, self.methods)
        name_oop = append_string_instance(vmem, self.class_name)
        super_name_oop = append_string_instance(vmem, self.super_name)

        oop = vmem.append_label_ref('CompiledClass', self.name)              # vt
        vmem.append_pointer_to(delegate)                                     # delegate
        vmem.append_pointer_to(name_oop)                                     # name
        vmem.append_pointer_to(super_name_oop)                               # super_class_name
        vmem.append_null()                                                   # compile_module: TODO
        vmem.append_pointer_to(fields_list_oop)                              # fields
        vmem.append_pointer_to(methods_oop)                                  # methods: TODO
        vmem.append_pointer_to(own_methods_oop)                              # own methods: TODO
        return oop

class CompiledFunction(Entry):
    def __init__(self, owner_name, name, params, prim_name):
        self.owner_name = owner_name
        self.name = name
        self.params = params
        self.prim_name = prim_name

    def fill(self, vmem):
        delegate = append_object_instance(vmem)
        name_oop = append_string_instance(vmem, self.name)
        params_list_oop = append_list_of_strings(vmem, self.params)
        prim_name_oop = append_string_instance(vmem, self.prim_name)

        label = utils.compiled_fun_name(self.owner_name, self.name)

        oop = vmem.append_label_ref('CompiledFunction', label)          # vt
        vmem.append_pointer_to(delegate)                                # delegate
        vmem.append_pointer_to(name_oop)                                # name
        vmem.append_pointer_to(params_list_oop)                         # params
        vmem.append_pointer_to(prim_name_oop)                           # prim_name
        vmem.append_label_ref(self.owner_name)                          # owner
        return oop

class Function(Entry):
    def __init__(self, owner_name, name, params, prim_name):
        self.owner_name = owner_name
        self.name = name
        self.params = params
        self.prim_name = prim_name

    def fill(self, vmem):
        delegate = append_object_instance(vmem)
        oop = vmem.append_label_ref('Function')                         # vt
        vmem.append_pointer_to(delegate)                                # delegate
        vmem.append_label_ref(
            utils.compiled_fun_name(self.owner_name, self.name))        # compiled_function
        vmem.append_null()                                              # module: TODO
        return oop


## instance entries


def append_object_instance(vmem):
    oop = vmem.append_label_ref('Object') # vt
    vmem.append_null()                    # delegate: end of chain of delegation
    return oop


def append_string_instance(vmem, string):
    delegate = append_object_instance(vmem) # Assumed to be object! if source change, this breaks
    oop = vmem.append_label_ref('String')   # vt
    vmem.append_pointer_to(delegate)        # delegate
    vmem.append_int(len(string))
    vmem.append_string(string)
    return oop


def append_empty_dict(vmem):
    delegate = append_object_instance(vmem)   # Assumed to be object! if source change, this breaks
    oop = vmem.append_label_ref('Dictionary') # vt
    vmem.append_pointer_to(delegate)          # delegate
    vmem.append_int(0)                        # dict length
    return oop

def append_entries_dict(vmem, entries_pydict):
    pairs_oop = []
    for key, entry, in entries_pydict.iteritems():
        pairs_oop.append(append_string_instance(vmem, key))
        pairs_oop.append(entry.fill(vmem))

    delegate = append_object_instance(vmem)   # Assumed to be object! if source change, this breaks
    oop = vmem.append_label_ref('Dictionary') # vt
    vmem.append_pointer_to(delegate)          # delegate
    vmem.append_int(len(entries_pydict))      # dict length
    for oop in pairs_oop:
        vmem.append_pointer_to(oop)
    return oop


def append_empty_list(vmem):
    delegate = append_object_instance(vmem)   # Assumed to be object! if source change, this breaks
    oop = vmem.append_label_ref('List')       # vt
    vmem.append_pointer_to(delegate)          # delegate
    vmem.append_int(0)                        # len
    return oop


# used internally to create class fields list, etc.
def append_list_of_strings(vmem, lst):
    oops_elements = [append_string_instance(vmem, string) for string in lst]
    delegate = append_object_instance(vmem)   # Assumed to be object! if source change, this breaks
    oop = vmem.append_label_ref('List')       # vt
    vmem.append_pointer_to(delegate)          # delegate
    vmem.append_int(len(lst))                 # len
    for oop in oops_elements:                   # .. elements
        vmem.append_pointer_to(oop)
    return oop


class Compiler(ASTBuilder):
    def __init__(self):
        self.entries = []
        self.current_object = None

        self.vmem = vmem.VirtualMemory()
        self.HEADER_SIZE = 3 * utils.WSIZE # bytes. 3 = names_size, entries, addr_table_offset

    def compile(self):
        self.line_offset = 0
        self.parser = MemeParser(open(os.path.join(os.path.dirname(__file__), 'core.md'), 'r').read())
        self.parser.i = self
        ast = self.parser.apply("start")[0]
        self.parser = CoreTr([ast])
        self.parser.i = self
        self.parser.apply('start')

        core = self.build_core()
        self.dump(core)

    ######################

    def register_object(self, name):
        self.current_object = ObjectEntry(name)
        self.entries.append(self.current_object)

    def register_class(self, name, super_class):
        # registering the behavior for that class
        behavior = BehaviorEntry(name, super_class, None)
        self.entries.append(behavior)

        # registering the compiled class for that class
        cclass = CompiledClassEntry(name, super_class)
        self.entries.append(cclass)

        self.current_class = ClassEntry(name, super_class, behavior, cclass)
        self.entries.append(self.current_class)

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
        self.current_class.add_method(
            Function(self.current_class.get_name(), name, params, primitive_name))

        self.current_class.compiled_class().add_method(
            name, CompiledFunction(self.current_class.get_name(), name, params, primitive_name))

    #################################################


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
        core['names'] = [(name_t, utils.string_block_size(name_t)) for name_t in [e.get_name() + "\0" for e in self.entries]]

        core['header']['names_size'] = sum([x[1] for x in core['names']])

        index_size = core['header']['entries'] * 2 * utils.WSIZE # *2: pair (name, entry), *4: bytes
        base = self.HEADER_SIZE + core['header']['names_size'] + index_size
        self.vmem.set_base(base)

        # - OBJECT TABLE
        for entry in self.entries:
            entry.fill(self.vmem)

        core['object_table'] = self.vmem.object_table()

        # - HEADER ot_size
        core['header']['ot_size'] = len(core['object_table'])

        core['addr_table'] = self.vmem.addr_table()

        # - INDEX section
        for name in [x.get_name() for x in self.entries]:
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
