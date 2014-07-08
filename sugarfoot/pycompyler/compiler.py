from pyparsers.parser import MemeParser
from pyparsers.memetr import MemeTr
from pyparsers.astbuilder import *
from pyutils import bits
from pprint import pprint as P
import traceback
from pdb import set_trace as br
import os
import sys
import struct
from . import opcode
from . import comp_vmemory
from . import utils

class CompiledClass(object):
    def __init__(self, cmod, name, super_name, fields):
        self.cmod = cmod
        self.name = name
        self.super_name = super_name
        self.fields = fields
        self.instance_methods = {}
        self.class_methods = {}

    def label(self):
        return self.cmod.label() + '_' + self.name + "_CompiledClass"

    def new_ctor(self, name):
        fn = CompiledFunction(self.cmod, self, name, ctor=True)
        self.class_methods[name] = fn
        return fn

    def new_instance_method(self, name):
        fn = CompiledFunction(self.cmod, self, name)
        self.instance_methods[name] = fn
        return fn

    def new_class_method(self, name):
        fn = CompiledFunction(self.cmod, self, name)
        self.class_methods[name] = fn
        return fn

    def fill(self, vmem):
        # vt: CompiledClass
        # delegate: ...
        # module: ...
        # name: ...
        # super_class_name: ...
        # fields
        # methods
        # class_methods ...

        delegate = vmem.append_object_instance()
        oop_name = vmem.append_string_instance(self.name)
        oop_super = vmem.append_string_instance(self.super_name)
        oop_fields = vmem.append_list_of_strings(self.fields)
        oop_methods = vmem.append_dict_emiting_entries(self.instance_methods)
        oop_class_methods = vmem.append_dict_emiting_entries(self.class_methods)

        oop = vmem.append_external_ref('CompiledClass', self.label()) # vt: CompiledClass
        vmem.append_pointer_to(delegate)
        vmem.append_label_ref(self.cmod.label())
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_super)
        vmem.append_pointer_to(oop_fields)
        vmem.append_pointer_to(oop_methods)
        vmem.append_pointer_to(oop_class_methods)
        return oop

class CompiledFunction(object):

    def __init__(self, cmod, owner, name, ctor=False):
        self.cmod = cmod
        self.name = name
        self.literal_frame = []
        self.bytecodes = []
        self.is_prim = False
        self.prim_name = ''
        self.owner = owner
        self.is_ctor = ctor
        self.has_env = False
        self.local_vars = []

    def uses_env(self, val):
        self.has_env = val

    def set_primitive(self, prim_name):
        self.prim_name = prim_name
        self.is_prim = True

    def label(self):
        return self.owner.label() + '_' + self.name + "_CompiledFunction"

    def literal_frame_label(self):
        return self.label() + '_literal_frame'

    def bytecode_label(self):
        return self.label() + '_bytecodes'

    def fill_literal_frame(self, vmem):
        if len(self.literal_frame) == 0:
            return 0

        # pre-append large objects
        lit_frame = []
        for lit in self.literal_frame:
            if lit['tag'] == 'number':
                lit_frame.append(('int', bits.tag(lit['value'])))
            elif lit['tag'] == 'symbol':
                oop = vmem.append_symbol_instance(lit['value'])
                lit_frame.append(('oop', oop))

        vmem.label_current(self.literal_frame_label())

        # fill frame
        for tp, val in lit_frame:
            if tp == 'int':
                vmem.append_int(val)
            elif tp == 'oop':
                vmem.append_pointer_to(val)
            else:
                raise Exception('Todo')

        return len(lit_frame) * bits.WSIZE


    def fill_bytecodes(self, vmem):
        if len(self.bytecodes) == 0:
            return 0

        vmem.label_current(self.bytecode_label())
        vmem.append_string(''.join([bits.pack32(w) for w in self.bytecodes]))
        return len(self.bytecodes) * opcode.WORD_SIZE

    def fill(self, vmem):
        # vt: CompiledFunction
        # delegate
        # name
        # params
        # is_ctor
        # is_prim
        # prim_name
        # flags: normal, setter, getter
        # getter/setter field index
        # owner
        # num_locals / env_size
        # literal frame size
        # literal_frame ptr
        # bytecode size
        # bytecode ptr

        # todo:
        #   uses env / env size
        #   num_locals

        oop_delegate = vmem.append_object_instance()
        oop_name = vmem.append_string_instance(self.name)
        oop_params = vmem.append_list_of_strings(self.params)
        oop_prim_name = vmem.append_string_instance(self.prim_name)

        lit_frame_size = self.fill_literal_frame(vmem)
        bytecode_size = self.fill_bytecodes(vmem)

        oop = vmem.append_external_ref('CompiledFunction', self.label()) # CompiledFunction vt
        vmem.append_pointer_to(oop_delegate)
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_params)
        vmem.append_int(int(self.is_ctor))
        vmem.append_int(int(self.is_prim))
        vmem.append_pointer_to(oop_prim_name)

        vmem.append_int(0) # normal=0/getter=1/setter=2
        vmem.append_int(0) # getter/setter field index

        vmem.append_label_ref(self.owner.label())

        vmem.append_int(len(self.params))
        if self.has_env:
            raise Exception('Todo')
        else:
            vmem.append_int(len(self.local_vars))

        vmem.append_int(lit_frame_size)
        if lit_frame_size > 0:
            vmem.append_label_ref(self.literal_frame_label())
        else:
            vmem.append_null()

        vmem.append_int(bytecode_size)
        if bytecode_size > 0:
            vmem.append_label_ref(self.bytecode_label())
        else:
            vmem.append_null()

        return oop

    def set_parameters(self, params):
        self.params = params

    def index_for_literal(self, entry):
        if entry not in self.literal_frame:
            self.literal_frame.append(entry)
        return self.literal_frame.index(entry)

    def create_and_register_number_literal(self, num):
        entry = {"tag": "number", "value": num}
        return self.index_for_literal(entry)

    def create_and_register_symbol_literal(self, string):
        entry = {"tag": "symbol", "value": string}
        return self.index_for_literal(entry)

    def index_and_popop_for(self, name):
        if name in self.params:
            return 'pop_param', self.params.index(name)
        if name not in self.local_vars:
            self.local_vars.append(name)
        return 'pop_local', self.local_vars.index(name)

    def index_and_pushop_for(self, name):
        if name in self.params:
            return 'push_param', self.params.index(name)
        if name not in self.local_vars:
            self.local_vars.append(name)
        return 'push_local', self.local_vars.index(name)

    def emit_push_num_literal(self, num):
        idx = self.create_and_register_number_literal(num)
        self.bytecodes.append(opcode.bytecode_for("push_literal", idx))

    def emit_push_var(self, name):
        #TODO: module param, fun param
        if self.has_env:
            idx = self.local_vars.index(name)
            self.bytecodes.append(opcode.bytecode_for("push_env",idx))
        else:
            opname, idx = self.index_and_pushop_for(name)
            self.bytecodes.append(opcode.bytecode_for(opname,idx))

    def emit_return_top(self):
        self.bytecodes.append(opcode.bytecode_for("ret_top",0))

    def emit_return_this(self):
        self.bytecodes.append(opcode.bytecode_for("ret_this",0))

    def emit_var_decl(self, name):
        if self.has_env:
            idx = self.add_env(name)
            self.bytecodes.append(opcode.bytecode_for("pop_env",idx))
        else:
            opname, idx = self.index_and_popop_for(name)
            self.bytecodes.append(opcode.bytecode_for(opname,idx))

    def emit_send_or_local_call(self, name, arity):
        if name in self.local_vars:
            raise Exception('todo')
        elif name in self.params:
            raise Exception('todo')
        elif name in self.cmod.functions:
            idx = self.create_and_register_symbol_literal(name)
            self.bytecodes.append(opcode.bytecode_for('push_module', 0))
            self.bytecodes.append(opcode.bytecode_for('push_literal', idx))
            self.bytecodes.append(opcode.bytecode_for('send', arity))
        elif name in self.cmod.params:
            raise Exception('todo')
        else:
            raise Exception('todo')
        # if name in module.params ...
        # if name in module entries
        # if name in self.params ...
        # if name in self.local vars ...
        # if name in self.env ...

class CompiledModule(object):
    def __init__(self, name):
        self.name = name
        self.params = []
        self.functions = {}
        self.classes = {}

    def label(self):
        return self.name + '_CompiledModule'

    def num_top_level_entries(self):
        return len(self.functions) + len(self.classes)

    def new_function(self, name):
        fn = CompiledFunction(self, self, name)
        self.functions[name] = fn
        return fn

    def new_class(self, name, parent, fields):
        klass = CompiledClass(self, name, parent, fields)
        self.classes[name] = klass
        return klass

    def fill(self, vmem):
        # first word on object table is a pointer to the CompiledModule
        vmem.append_label_ref(self.label())

        delegate = vmem.append_object_instance()
        oop_name = vmem.append_string_instance(self.name)
        oop_license = vmem.append_string_instance("")
        oop_params = vmem.append_list_of_strings([])
        oop_functions = vmem.append_dict_emiting_entries(self.functions)
        oop_classes = vmem.append_dict_emiting_entries(self.classes)

        oop = vmem.append_external_ref('CompiledModule', self.label()) # vt: CompiledModule
        vmem.append_pointer_to(delegate)
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_license)
        vmem.append_pointer_to(oop_params)
        vmem.append_pointer_to(oop_functions)
        vmem.append_pointer_to(oop_classes)
        return oop


class MMC(object):
    MAGIC_NUMBER = 0x420
    HEADER_SIZE = 4 * bits.WSIZE

    def __init__(self, module):
        self.module = module

    def name_ptr_for(self, name, mmc):
        acc = 0
        for entry_name_t, bsize in mmc['names']:
            if entry_name_t[0:-1] == name:
                return self.HEADER_SIZE + acc
            acc += bsize
        raise Exception('entry {} not found in NAMES'.format(name))

    def create_mmc_struct(self, vmem):
        mmc = {'header':
               {'magic_number': None,
                'ot_size': None,
                'es_size': None,
                'names_size': None},
               'names': [],
               'object_table': [],
               'external_symbols': [],
               'reloc_table': []
            }


        self.module.fill(vmem)

        mmc['header']['magic_number'] = self.MAGIC_NUMBER


        mmc['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in [name + '\0' for name in vmem.external_names()]]
        mmc['header']['names_size'] = sum([x[1] for x in mmc['names']])

        base = self.HEADER_SIZE + mmc['header']['names_size']
        vmem.set_base(base)

        mmc['object_table'] = vmem.object_table()
        mmc['header']['ot_size'] = len(mmc['object_table'])

        for pair in vmem.external_symbols():
            mmc['external_symbols'].append(self.name_ptr_for(pair[0], mmc))
            mmc['external_symbols'].append(pair[1])

        mmc['header']['es_size'] = len(mmc['external_symbols']) * bits.WSIZE

        mmc['reloc_table'] = vmem.reloc_table()

        return mmc

    def dump(self, vmem):
        mmc = self.create_mmc_struct(vmem)
        with open(self.module.name + ".mmc", "w") as fp:
            # header
            fp.write(bits.pack_word(mmc['header']['magic_number']))
            fp.write(bits.pack_word(mmc['header']['ot_size']))
            fp.write(bits.pack_word(mmc['header']['es_size']))
            fp.write(bits.pack_word(mmc['header']['names_size']))

            # names
            for name, chunk_size in mmc['names']:
                text = name + ((chunk_size - len(name)) * '\0')
                fp.write(text)

            # object table
            for v in mmc['object_table']:
                fp.write(bits.pack_byte(v))

            # external symbols
            for v in mmc['external_symbols']:
                fp.write(bits.pack_word(v))

            # reloc table
            for word in mmc['reloc_table']:
                fp.write(bits.pack_word(word))


class Compiler(ASTBuilder):
    def __init__(self):
        self.cmodule = None
        self.filepath = None

    def do_parse(self, parser):
        try:
            parser.uses_literal = utils.Flag() # pymeta uses eval() which disables assignment. This works around it
            return parser.apply("start")
        except Exception as err:
            if hasattr(err,'formatError'):
                print err.formatError(''.join(parser.input.data))
            else:
                print err, traceback.format_exc()
            sys.exit(1)

    def compile(self, filepath):
        self.filepath = filepath
        self.line_offset = 0
        self.parser = MemeParser(open(filepath, 'r').read())
        self.parser.i = self

        ast = self.do_parse(self.parser)[0]

        print ast

        self.parser = MemeTr([ast])
        self.parser.i = self

        self.do_parse(self.parser)

        vmem = comp_vmemory.CompVirtualMemory()
        mmc = MMC(self.cmodule)
        mmc.dump(vmem)

    def new_module(self):
        module_name =  os.path.splitext(os.path.basename(self.filepath))[0]
        self.cmodule = CompiledModule(module_name)
        return self.cmodule

Compiler().compile(sys.argv[1])
