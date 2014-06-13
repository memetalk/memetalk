from parser import MemeParser
from coretr import CoreTr
from astbuilder import *
import etable
import math
from pprint import pprint as P
from pdb import set_trace as br
import struct
import ctypes

class Entry(object):
    def fill(self, etable):
        raise Exception("Implement me")


class ObjectEntry(Entry):
    def __init__(self, name):
        self.name = name
        self.slots = []

    def get_name(self):
        return self.name

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

    def fill(self, etable):
        self.emit_slot(etable, self.slots[0], self.name)
        for slot in self.slots[1:]:
            self.emit_slot(etable, slot)

    def emit_slot(self, etable, slot, label=None):
        if slot['type'] == 'ref':
            return etable.append_label_ref(slot['value'], label) # Behavior needs this
        elif slot['type'] == 'null':
            return etable.append_null()
        elif slot['type'] == 'string':
            return etable.append_string(slot['value'])
        elif slot['type'] == 'empty_list':
            return append_empty_list(etable)
        elif slot['type'] == 'empty_dict':
            return append_empty_dict(etable)
        elif slot['type'] == 'int':
            return etable.append_int(slot['value'])
        else:
            raise Exception('TODO')


class ClassEntry(Entry):
    def __init__(self, name, super_class, behavior_entry, cclass_entry):
        self.name = name
        self.super_class = super_class
        self.behavior_entry = behavior_entry
        self.cclass_entry = cclass_entry
        if super_class != 'Object':
            raise Exception('TODO')

    def get_name(self):
        return self.name

    def set_fields(self, fields):
        self.cclass_entry.set_fields(fields)

    def fill(self, etable):
        oop_dict = append_empty_dict(etable)
        delegate = append_object_instance(etable)

        etable.append_label_ref(self.name + 'Behavior', self.name) # vt
        etable.append_pointer_to(delegate)                         # delegate
        etable.append_label_ref(self.super_class)                  # parent
        etable.append_pointer_to(oop_dict)                         # dict: "methods"
        etable.append_label_ref(self.name + "_CompiledClass")      # compiled_class


class BehaviorEntry(Entry):
    def __init__(self, name, parent_name, dictionary):
        self.name = name
        self.parent_name = parent_name
        self.dictionary = dictionary
        if parent_name != 'Object':
            raise Exception('TODO')

    def get_name(self):
        return self.name + 'Behavior'

    def fill(self, etable):
        delegate = append_object_instance(etable)
        oop_dict = append_empty_dict(etable)
        etable.append_label_ref('Behavior', self.name + 'Behavior') # vt
        etable.append_pointer_to(delegate)                          # delegate
        etable.append_label_ref(self.parent_name + "Behavior")      # parent
        etable.append_pointer_to(oop_dict)                          # dict: "own methods"


class CompiledClassEntry(Entry):
    def __init__(self, name, super_name):
        self.name = name
        self.super_name = super_name
        self.fields = []

    def get_name(self):
        return self.name + '_CompiledClass'

    def set_fields(self, fields):
        self.fields = fields

    def fill(self, etable):
        delegate = append_object_instance(etable)
        fields_list_oop = append_list_of_strings(etable, self.fields)
        own_methods_oop = append_empty_dict(etable)
        methods_oop = append_empty_dict(etable)
        etable.append_label_ref('CompiledClass', self.name + '_CompiledClass') # vt
        etable.append_pointer_to(delegate)                                     # delegate
        etable.append_string(self.name)                                        # name
        etable.append_string(self.super_name)                                  # super_class_name
        etable.append_null()                                                   # compile_module: TODO
        etable.append_pointer_to(fields_list_oop)                              # fields
        etable.append_pointer_to(methods_oop)                                  # methods: TODO
        etable.append_pointer_to(own_methods_oop)                              # own methods: TODO

## instance entries


def append_object_instance(etable):
    oop = etable.append_label_ref('Object') # vt
    etable.append_null()                    # delegate: end of chain of delegation
    return oop


def append_string_instance(etable, string):
    delegate = append_object_instance(etable) # Assumed to be object! if source change, this breaks
    oop = etable.append_label_ref('String')   # vt
    etable.append_pointer_to(delegate)        # delegate
    etable.append_string(string)
    return oop


def append_empty_dict(etable):
    delegate = append_object_instance(etable)   # Assumed to be object! if source change, this breaks
    oop = etable.append_label_ref('Dictionary') # vt
    etable.append_pointer_to(delegate)          # delegate
    etable.append_int(0)                        # dict length
    return oop


def append_empty_list(etable):
    delegate = append_object_instance(etable)   # Assumed to be object! if source change, this breaks
    oop = etable.append_label_ref('List')       # vt
    etable.append_pointer_to(delegate)          # delegate
    etable.append_int(0)                        # len
    return oop


# used internally to create class fields list, etc.
def append_list_of_strings(etable, lst):
    oops_elements = [etable.append_string(string) for string in lst]
    delegate = append_object_instance(etable)   # Assumed to be object! if source change, this breaks
    oop = etable.append_label_ref('List')       # vt
    etable.append_pointer_to(delegate)          # delegate
    etable.append_int(len(lst))                 # len
    for oop in oops_elements:                   # .. elements
        etable.append_pointer_to(oop)
    return oop


class Compiler(ASTBuilder):
    def __init__(self):
        self.entries = []
        self.current_object = None

        self.etable = etable.VirtualEntryTable()
        self.HEADER_SIZE = 3 * 4 # bytes. 3 = names_size, entries, addr_table_offset

    def compile(self):
        self.line_offset = 0
        self.parser = MemeParser(open('core.md').read())
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


    ###########

    def name_ptr_for_name(self, name, core):
        acc = 0
        for entry_name in core['names']:
            if entry_name[0] == name:
                return self.HEADER_SIZE + acc
            acc += entry_name[1]
        raise Exception('entry {} not found in NAMES'.format(name))

    def build_core(self):
        core = {
            'header': {
                'entries': None,             # number of top level objects
                'names_size': None,          # size in bytes of NAMES section
                'ot_size': None},            # size of OBJECT TABLE in bytes
            'names': [],
            'index': [],
            'object_table': [],
            'addr_table': []}

        # -- HEADER and NAMES section
        core['header']['entries'] = len(self.entries)

        # names :: [(string, alloc-size in bytes, self-ptr)]
        core['names'] = [(entry.get_name(),
                          int(math.ceil((len(entry.get_name()) + 1) * 8 / 32.0)) * 4) for entry in self.entries] # +1: "\0"

        core['header']['names_size'] = sum([x[1] for x in core['names']])
        core['header']['entries'] = len(self.entries)

        index_size = core['header']['entries'] * 2 * 4 # *2: pair (name, entry), *4: bytes
        base = self.HEADER_SIZE + core['header']['names_size'] + index_size
        self.etable.set_base(base)

        # - OBJECT TABLE
        for entry in self.entries:
            entry.fill(self.etable)

        core['object_table'] = self.etable.object_table()

        # - HEADER ot_size
        core['header']['ot_size'] = len(core['object_table']) * 4

        core['addr_table'] =self.etable.addr_table()

        # - INDEX section
        top_level_names = [x.name for x in self.entries]
        for name in top_level_names:
            core['index'].append(self.name_ptr_for_name(name, core))  # ptr to string
            core['index'].append(self.etable.index[name])             # ptr to object

        return core
        # # br()
        # for idx, val in enumerate(core['object_table']):
        #     print idx + 100, ':', val

    def dump(self, core):
        P(core['header'])

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
        for v32 in core['object_table']:
            fp.write(struct.pack('I', v32))
            # except Exception as e:
            #     br()
            #     fp.write(struct.pack('I', ctypes.c_uint32(~v32).value))

        # addr table
        for v32 in core['addr_table']:
            fp.write(struct.pack('i', v32))

        fp.close()

Compiler().compile()
