from pyparsers.parser import MemeParser
from pyparsers.coretr import CoreTr
from pyparsers.astbuilder import *
from pyutils import bits
from . import core_vmem
from . import utils
import math
import struct
import ctypes
from pprint import pprint as P
from pdb import set_trace as br
import os


class Entry(object):
    def fill(self, vmem):
        raise Exception("Implement me")

class ObjectEntry(Entry):
    def __init__(self, c, name):
        self.compiler = c
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

    def fill(self, vmem):
        # synth necessary objects:
        refs_to_literals = {}
        for idx, slot in enumerate(self.slots[1:]):
            if slot['type'] == 'string':
                refs_to_literals[idx] = vmem.append_string_instance(slot['value'])
            elif slot['type'] == 'empty_list':
                refs_to_literals[idx] = vmem.append_empty_list()
            elif slot['type'] == 'empty_dict':
                refs_to_literals[idx] = vmem.append_empty_dict()

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
                vmem.append_tagged_int(slot['value'])
            else:
                raise Exception('TODO')
        return oop

class ClassEntry(Entry):
    def __init__(self, c, name, super_class, behavior_entry, cclass_entry):
        self.compiler = c
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

    def fill(self, vmem):
        oop_dict = vmem.append_dict_emiting_entries(self.dictionary)
        delegate = vmem.append_object_instance()

        oop = vmem.append_label_ref(utils.behavior_label(self.name),  self.label)  # vt
        vmem.append_pointer_to(delegate)                                           # delegate
        vmem.append_label_ref(self.super_class)                                    # parent
        vmem.append_pointer_to(oop_dict)                                           # dict: "methods"
        vmem.append_label_ref(utils.cclass_label(self.name))                       # compiled_class
        return oop

class BehaviorEntry(Entry):
    def __init__(self, c, name, parent_name, dictionary):
        self.compiler = c
        self.name = name
        self.label = utils.behavior_label(name)
        self.parent_name = parent_name
        self.parent_label = utils.behavior_label(parent_name)
        self.dictionary = dictionary
        if parent_name != 'Object':
            raise Exception('TODO')

    def fill(self, vmem):
        delegate = vmem.append_object_instance()
        oop_dict = vmem.append_empty_dict()
        oop = vmem.append_label_ref('Behavior', self.label)       # vt
        vmem.append_pointer_to(delegate)                          # delegate
        vmem.append_label_ref(self.parent_label)                  # parent
        vmem.append_pointer_to(oop_dict)                          # dict: "own methods"
        return oop


class CompiledClassEntry(Entry):
    def __init__(self, c, name, super_name):
        self.compiler = c
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

    def fill(self, vmem):
        delegate = vmem.append_object_instance()
        fields_list_oop = vmem.append_list_of_strings(self.fields)
        own_methods_oop = vmem.append_empty_dict()
        methods_oop = vmem.append_dict_emiting_entries(self.methods)
        name_oop = vmem.append_string_instance(self.class_name)
        super_name_oop = vmem.append_string_instance(self.super_name)

        oop = vmem.append_label_ref(utils.class_label('CompiledClass'), self.label)             # vt
        vmem.append_pointer_to(delegate)                                     # delegate
        vmem.append_pointer_to(name_oop)                                     # name
        vmem.append_pointer_to(super_name_oop)                               # super_class_name
        vmem.append_label_ref('@core_compiled_module')                       # compiled_module
        vmem.append_pointer_to(fields_list_oop)                              # fields
        vmem.append_pointer_to(methods_oop)                                  # methods: TODO
        vmem.append_pointer_to(own_methods_oop)                              # own methods: TODO
        return oop

class CompiledFunction(Entry):
    def __init__(self, c, owner_name, name, params, prim_name):
        self.compiler = c
        self.owner_name = owner_name
        self.name = name
        self.label = utils.cfun_label(owner_name, name)
        self.params = params
        self.prim_name = prim_name

    def fill(self, vmem):
        delegate = vmem.append_object_instance()
        name_oop = vmem.append_string_instance(self.name)
        params_list_oop = vmem.append_list_of_strings(self.params)
        prim_name_oop = vmem.append_string_instance(self.prim_name)

        label = utils.cfun_label(self.owner_name, self.name)

        oop = vmem.append_label_ref(utils.class_label('CompiledFunction'), self.label)  # vt
        vmem.append_pointer_to(delegate)                          # delegate
        vmem.append_pointer_to(name_oop)                          # name
        vmem.append_pointer_to(params_list_oop)                   # params
        vmem.append_pointer_to(prim_name_oop)                     # prim_name
        vmem.append_label_ref(self.owner_name)                    # owner [CompiledClass/CompiledModule]
        return oop

class Function(Entry):
    def __init__(self, c, cfun, name):
        self.compiler = c
        self.cfun = cfun
        self.name = name
        self.label = utils.fun_label(self.cfun.owner_name, self.name)

    def fill(self, vmem):
        delegate = vmem.append_object_instance()
        oop = vmem.append_label_ref('Function', self.label)     # vt
        vmem.append_pointer_to(delegate)                        # delegate
        vmem.append_label_ref(self.cfun.label)                  # compiled_function
        vmem.append_label_ref('@core_module')                   # module
        return oop

class CoreCompiledModule(Entry):
    # core's CompiledModule instance
    def __init__(self, c):
        self.compiler = c
        self.label = '@core_compiled_module'

    def compiled_functions_dict(self, vmem):
        # this assumes the cfun were not emited (ie, they are not part of c.entries)
        # -this is the place that emits module cfuns!

        cfun_entries = dict([(name, fun.cfun) for name, fun in self.compiler.top_level_functions.iteritems()])
        return vmem.append_dict_emiting_entries(cfun_entries)

        # for fun_entry in c.top_level_functions:
        #     keys.append(append_string_instance(c, fun_entry.name))

        # dict_oop = _append_dict_prologue(c, len(c.top_level_functions))
        # for idx, fun_entry in enumerate(c.top_level_functions):
        #     c.vmem.append_pointer_to(keys[idx])
        #     c.vmem.append_label_ref(fun_entry.cfun.label)
        # return dict_oop

    def compiled_classes_dict(self, vmem):
        # shit, this is complex. We can't emit the classes again so...
        # -first, emit string instances for the dict keys
        # -then, assembly the dict object and its pairs manually

        keys = []
        for entry in self.compiler.top_level_classes:
            keys.append(vmem.append_string_instance(entry.label))

        dict_oop = vmem._append_dict_prologue(len(self.compiler.top_level_classes))
        for idx, entry in enumerate(self.compiler.top_level_classes):
            vmem.append_pointer_to(keys[idx])
            vmem.append_label_ref(entry.label)
        return dict_oop

    def fill(self, vmem):
        # vt, delegate, name, license, params
        # compiled_functions, compiled_classes
        # parent_module: <CompiledModule>

        delegate = vmem.append_object_instance()
        name_oop = vmem.append_string_instance('core')
        license_oop = vmem.append_string_instance('')
        cfuns_dict = self.compiled_functions_dict(vmem)
        cclass_dict = self.compiled_classes_dict(vmem)

        oop = vmem.append_label_ref('CompiledModule', self.label)      # vt
        vmem.append_pointer_to(delegate)                               # delegate
        vmem.append_pointer_to(name_oop)                               # name
        vmem.append_pointer_to(license_oop)                            # license
        vmem.append_null()                                             # params
        vmem.append_pointer_to(cfuns_dict)                             # compiled_functions
        vmem.append_pointer_to(cclass_dict)                            # compiled_classes
        vmem.append_null()                                             # parent_module
        return oop

class CoreModule(Entry):
    # core's Module instance (conflated approach)
    def __init__(self, c):
        self.compiler = c
        self.label = '@core_module'

    def create_dict(self, vmem):
        # dict (TODO)
        # -Functions of the module
        return vmem.append_dict_emiting_entries(self.compiler.top_level_functions)

        # pairs = []
        # for fun_entry in self.compiler.top_level_functions.values():
        #     oop_key = vmem.append_string_instance(fun_entry.name)
        #     oop_val = fun_entry.fill(vmem)
        #     pairs.append((oop_key, oop_val))
        # # -acessors for compiled classes
        # for cclass in self.compiler.top_level_classes:
        #     cfun = CompiledFunction(self.compiler, '@core_compiled_module', cclass.name, [], 'prim_get_' + cclass.name)
        #     cfun.fill(c)
        #     fun = Function(cfun, cclass.name)
        #     oop_key = vmem.append_string_instance(cclass.name)
        #     oop_val = fun.fill(c)
        #     pairs.append((oop_key, oop_val))

    def fill(self, vmem):
        # vt, delegate, parent, dict, compiled_module, [entries]
        oop_dict = self.create_dict(vmem)
        oop = vmem.append_label_ref(self.label, self.label)     # vt
        vmem.append_null()                                      # delegate
        vmem.append_null()                                      # parent
        vmem.append_pointer_to(oop_dict)                        # dict
        vmem.append_label_ref('@core_compiled_module')          # compiled_module
        # for name in c.top_levels:
        #     c.vmem.append_label_ref(name)                         # compiled_module
        return oop



class Compiler(ASTBuilder):
    def __init__(self):
        self.prime_names = []
        self.top_level_functions = {}
        self.top_level_classes = []
        self.top_levels = []

        self.entries = []
        self.current_object = None

        self.vmem = core_vmem.CoreVirtualMemory()
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
        self.current_object = ObjectEntry(self, name)
        self.entries.append(self.current_object)
        # there is no "class Object", so...
        if name == 'Object':
            self.top_level_classes.append(self.current_object)

    def register_class(self, name, super_class):
        self.top_levels.append(name)
        self.prime_names.append(name)
        # registering the behavior for that class
        behavior = BehaviorEntry(self, name, super_class, None)
        self.entries.append(behavior)

        # registering the compiled class for that class
        cclass = CompiledClassEntry(self, name, super_class)
        self.entries.append(cclass)

        self.current_class = ClassEntry(self, name, super_class, behavior, cclass)
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
        cfun = CompiledFunction(self, self.current_class.cclass_entry.label, name, params, primitive_name)

        self.current_class.add_method(Function(self, cfun, name))

        self.current_class.compiled_class().add_method(name, cfun)

    def add_module_function(self, name, params, body_ast):
        primitive_name =  body_ast[0][1][1]
        cfun = CompiledFunction(self, '@core_compiled_module', name, params, primitive_name)
        fun = Function(self, cfun, name)
        self.top_level_functions[name] = fun

    #################################################

    def synth_core_module(self):
        self.entries.append(CoreCompiledModule(self))
        self.entries.append(CoreModule(self))

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
            'reloc_table': []}

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
            entry.fill(self.vmem)

        core['object_table'] = self.vmem.object_table()

        # - HEADER ot_size
        core['header']['ot_size'] = len(core['object_table'])

        core['reloc_table'] = self.vmem.reloc_table()

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
        for v32 in core['reloc_table']:
            fp.write(struct.pack('I', v32))

        fp.close()

Compiler().compile()
