# -Make all "dict" point to a dict instance
# -For each entry so far, add the slots that have literals as value
# -write disassembler of core to debug it.
# -do functions for objects
# -do methods

from parser import MemeParser
from coretr import CoreTr
from astbuilder import *
import etable
import math
from pprint import pprint as P
from pdb import set_trace as br


class Entry(object):
    def fill(self, etable):
        raise Exception("Implement me")

class ObjectEntry(Entry):
    def __init__(self, name):
        self.name = name
        self.slots = []

    def add_slot_ref(self, name, value):
        self.slots.append({'type': 'ref', 'name': name, 'value': value})

    def add_slot_literal_null(self, name):
        self.slots.append({'type': 'null', 'name': name})

    def fill(self, etable):
        self.emit_slot(etable, self.slots[0], self.name)
        for slot in self.slots[1:]:
            self.emit_slot(etable, slot)

    def emit_slot(self, etable, slot, label=None):
        if slot['type'] == 'ref':
            return etable.append_label_ref(slot['value'], label)
        elif slot['type'] == 'null':
            return etable.append_int(0)
        else:
            raise Exception('TODO')


class ClassEntry(Entry):
    def __init__(self, name, super_class):
        self.name = name
        self.super_class = super_class
        if super_class != 'Object':
            raise Exception('TODO')

    def fill(self, etable):
        oop_dict = append_empty_dict(etable)
        delegate = append_object_instance(etable)

        etable.append_label_ref(self.name + 'Behavior', self.name) # vt
        etable.append_pointer_to(delegate)                         # delegate
        etable.append_label_ref(self.super_class)                  # parent
        etable.append_pointer_to(oop_dict)                         # dict
        etable.append_label_ref(self.name + "_CompiledClass")      # compiled_class


class BehaviorEntry(Entry):
    def __init__(self, name, parent_name, dictionary):
        self.name = name + 'Behavior'
        self.parent_name = parent_name
        self.dictionary = dictionary

    def fill(self, etable):
        delegate = append_object_instance(etable)
        oop_dict = append_empty_dict(etable)
        etable.append_label_ref('Behavior', self.name)         # vt
        etable.append_pointer_to(delegate)                     # delegate
        etable.append_label_ref(self.parent_name + "Behavior") # parent
        etable.append_pointer_to(oop_dict)                     # dict

class CompiledClassEntry(Entry):
    def __init__(self, name):
        self.name = name + '_CompiledClass'

    def fill(self, etable):
        etable.append_label_ref('CompiledClass', self.name) # vt
        etable.append_int(0)                                # delegate


## instance entries

# def append_object_behavior_instance(etable):
#     delegate = create_object_instance()
#     oop = etable.append_label_ref('Object') # vt
#     etable.append_int(delegate)             # delegate
#     return oop

def append_object_instance(etable):
    oop = etable.append_label_ref('Object') # vt
    etable.append_int(0)                    # delegate: end of chain of delegation
    return oop


def append_string_instance(etable, string):
    delegate = append_object_instance(etable) # Assumed to be object. In the future, deduce from source code and create appropriate delegate chain
    oop = etable.append_label_ref('String')   # vt
    etable.append_int(delegate)                # delegate
    etable.append_string(string)
    return oop

def append_empty_dict(etable):
    delegate = append_object_instance(etable)   # Assumed to be object. In the future, deduce from source code and create appropriate delegate chain
    oop = etable.append_label_ref('Dictionary') # vt
    etable.append_pointer_to(delegate)          # delegate
    etable.append_int(0)                        # dict length
    return oop

# class DictInstanceEntry(Entry):
#     def __init__(self, d):
#         self.d = d

#     def fill(self, etable):
#         if len(self.d) > 0:
#             raise Exception('Double check if this works')

#         keyvals_oops = []
#         for key, val in self.d.iteritems():
#             keyvals_oops.append(create_string_instance(key))
#             keyvals_oops.append(val)

#         oop = etable.append_label_ref('Dictionary')
#         delegate = ObjectInstanceEntry().fill(etable)
#         etable.append_pointer_to(delegate)
#         etable.append_int(len(self.d))
#         for oop in keyvals_oops:
#             etable.append_pointer_to(oop)

class Compiler(ASTBuilder):
    def __init__(self):
        self.entries = []
        self.current_object = None

        self.etable = etable.VirtualEntryTable()

    def compile(self):
        self.line_offset = 0
        self.parser = MemeParser(open('core.md').read())
        self.parser.i = self
        ast = self.parser.apply("start")[0]
        self.parser = CoreTr([ast])
        self.parser.i = self
        self.parser.apply('start')

        self.dump()

    ######################

    def register_object(self, name):
        self.current_object = ObjectEntry(name)
        self.entries.append(self.current_object)

    def register_class(self, name, super_class):
        self.current_class = ClassEntry(name, super_class)
        self.entries.append(self.current_class)

        # registering the behavior for that class
        self.entries.append(BehaviorEntry(name, super_class, None))

        # registering the compiled class for that class
        self.entries.append(CompiledClassEntry(name))

    def add_class_fields(self, fields):
        if len(fields) != 0:
            raise Exception('TODO')

    def add_slot_ref(self, name, value):
        self.current_object.add_slot_ref(name, value)

    def add_slot_literal_null(self, name):
        self.current_object.add_slot_literal_null(name)


    ###########

    def dump(self):
        HEADER_SIZE = 3 * 4 # bytes. 3 = names_size, entries, addr_table_offset

        core = {
            'header': {
                'names_size': None,          # size in bytes of NAMES section
                'entries': None,             # number of top level objects
                'ot_size': None},            # size of OBJECT TABLE in bytes
            'names': [],
            'index': [],
            'object_table': [],
            'addr_table': []}

        # -- HEADER and NAMES section
        core['header']['entries'] = len(self.entries)

        # names :: [(string, alloc-size in bytes)]
        core['names'] = [(entry.name, int(math.ceil((len(entry.name) + 1) * 8 / 32.0)) * 4) for entry in self.entries] # +1: "\0"
        core['header']['names_size'] = sum([x[1] for x in core['names']])
        core['header']['entries'] = len(self.entries)

        # - INDEX section

        index_size = core['header']['entries'] * 2 * 4 # *2: pair (name, entry), *4: bytes
        base = HEADER_SIZE + core['header']['names_size'] + index_size
        # self.etable.set_base(100) # debuging
        self.etable.set_base(base)

        # - OBJECT TABLE
        for entry in self.entries:
            entry.fill(self.etable)

        core['object_table'] = self.etable.object_table()

        # -- HEADER ot_size
        core['header']['ot_size'] = len(core['object_table']) * 4

        core['addr_table'] =self.etable.addr_table()

        br()
        for idx, val in enumerate(core['object_table']):
            print idx + 100, ':', val

Compiler().compile()
