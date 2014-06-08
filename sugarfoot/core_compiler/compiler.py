# -Make all "dict" point to a dict instance
# -For each entry so far, add the slots that have literals as value
# -do functions for objects
# -do methods

from parser import MemeParser
from coretr import CoreTr
from astbuilder import *
import pprint

class Entry(object):
    def __init__(self):
        pass

class ClassEntry(Entry):
    def __init__(self, name, super_class):
        self.name = name
        self.super_class = super_class
        if super_class != 'Object':
            raise Exception('TODO')

    def fill(self, vt):
        vt.append_new_addr(self.name + 'Behavior') # _vt
        vt.append_null_value() # _delegate
        vt.append_new_addr(self.super_class) # parent
        vt.append_null_value() # dict
        vt.append_new_addr(self.name + '_CompiledClass') # compiled_class


class ObjectEntry(Entry):
    def __init__(self, name):
        self.name = name
        self.slots = []

    def add_slot_ref(self, name, value):
        self.slots.append({'type': 'ref', 'name': name, 'value': value})

    def add_slot_literal_null(self, name):
        self.slots.append({'type': 'null', 'name': name})

    def fill(self, vt):
        for slot in self.slots:
            if slot['type'] == 'ref':
                vt.append_new_addr(slot['value'])
            elif slot['type'] == 'null':
                vt.append_null_value()
            else:
                raise Exception('Not implemented')

class VirtualAddressTable(object):
    def __init__(self, base):
        self.obj_table = []
        self.base = base
        self.index = {}
        self.addr_table = []

    def address_table(self):
        return self.addr_table

    def object_table(self):
        return [x() for x in self.obj_table]

    def addr_table_ptr(self):
        return self.base

    def assign(self, name):
        self.index[name] = self.base

    def append_new_addr(self, name):
        vaddr = lambda: self.index[name]
        self.obj_table.append(vaddr)
        self.addr_table.append(self.base)
        self.base += 1

    def append_null_value(self):
        vaddr = lambda: 0
        self.obj_table.append(vaddr)
        self.base += 1


class Compiler(ASTBuilder):
    def __init__(self):
        self.primes_names = [
            'Behavior',
            'ObjectBehavior',
            'Object_CompiledClass',
            'Object',
            'CompiledClass',
            'CompiledFunction']

        self.primes = {}

        self.other_entries = []
        self.current_object = None

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
        if name in self.primes_names:
            self.primes[name] = self.current_object
        else:
            self.other_entries.append(self.current_object)

    def register_class(self, name, super_class):
        self.current_class = ClassEntry(name, super_class)
        if name in self.primes_names:
            self.primes[name] = self.current_class
        else:
            self.other_entries.append(self.current_class)

        # registering the behavior for that class
        behavior = ObjectEntry(name+"Behavior")
        behavior.add_slot_ref('_vt', 'Behavior')
        behavior.add_slot_literal_null('_delegate')
        behavior.add_slot_ref('parent', super_class+'Behavior')
        behavior.add_slot_literal_null('dict') # TODO: should be dict literal with class methods
        self.other_entries.append(behavior)

        # registering the compiled class for that class
        cclass = ObjectEntry(name+"_CompiledClass")
        cclass.add_slot_ref('_vt', 'CompiledClass')
        cclass.add_slot_literal_null('_delegate')
        # TODO: fields, methods, own_methods, module, name, super_class_name
        self.other_entries.append(cclass)

    def add_class_fields(self, fields):
        if len(fields) != 0:
            raise Exception('TODO')

    def add_slot_ref(self, name, value):
        self.current_object.add_slot_ref(name, value)

    def add_slot_literal_null(self, name):
        self.current_object.add_slot_literal_null(name)


    ###########

    def dump(self):
        HEADER_SIZE = 2 # index_size, addr_table

        core = {
            'header': {
                'index_size': None,
                'addr_table_ptr': None},
            'index': [],
            'object_table': [],
            'addr_table': []}

        core['header']['index_size'] = len(self.primes_names)

        vt = VirtualAddressTable(HEADER_SIZE + core['header']['index_size'])

        # --- object table
        # 1) primes
        for name in self.primes_names:
            if name not in self.primes:
                # raise Exception('core source is incomplete: missing prime {}'.format(name))
                continue
            vt.assign(name)
            self.primes[name].fill(vt)

        # 2) others
        for entry in self.other_entries:
            vt.assign(entry.name)
            entry.fill(vt)

        # end of computations

        core['addr_table'] = vt.address_table()
        core['object_table'] = vt.object_table()
        core['header']['addr_table_ptr'] = vt.addr_table_ptr()

        self.print_core(core)

    def print_core(self, core):
        pprint.pprint(core)
        print '---'
        for idx, x in enumerate(core['object_table']):
            print '[{}] - {}'.format(idx+8, x)

Compiler().compile()
