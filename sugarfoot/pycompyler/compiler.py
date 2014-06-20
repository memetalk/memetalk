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


class CompiledFunction(object):

    def __init__(self, owner, name):
        self.name = name
        self.literal_frame = []
        self.bytecodes = []
        self.is_prim = False
        self.prim_name = ''
        self.owner = owner

    def label(self):
        return self.owner.label() + '_' + self.name + "_CompiledFunction"

    def literal_frame_label(self):
        return self.label() + '_literal_frame'

    def fill_literal_frame(self, vmem):
        if len(self.literal_frame) == 0:
            return False

        vmem.label_current(self.literal_frame_label())

        for lit in self.literal_frame:
            if lit['tag'] == 'number':
                vmem.append_int(lit['value'])
            else:
                raise Exception('Todo')
        return True

    def fill(self, vmem):
        # vt: CompiledFunction
        # delegate: ...
        # name: ...
        # ...
        # literal_frame: <pointer to a block of memory where the literal table is>

        oop_delegate = vmem.append_object_instance()
        oop_name = vmem.append_string_instance(self.name)
        oop_params = vmem.append_list_of_strings(self.params)
        oop_prim_name = vmem.append_list_of_strings(self.prim_name)

        has_lit_frame = self.fill_literal_frame(vmem)

        oop = vmem.append_external_ref('CompiledFunction', self.label()) # CompiledFunction vt
        vmem.append_pointer_to(oop_delegate)
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_params)
        vmem.append_int(int(self.is_prim))
        vmem.append_pointer_to(oop_prim_name)
        vmem.append_label_ref(self.owner.label())
        vmem.append_int(len(self.literal_frame))

        if has_lit_frame:
            vmem.append_label_ref(self.literal_frame_label())
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

    def emit_push_num_literal(self, num):
        idx = self.create_and_register_number_literal(num)
        self.bytecodes.append(opcode.bytecode_for("push_literal", idx))

    def emit_return(self, ret):
        self.bytecodes.append(opcode.bytecode_for("ret_top",0))

    def emit_return_this(self):
        self.bytecodes.append(opcode.bytecode_for("ret_this",0))

class CompiledModule(object):
    def __init__(self, name):
        self.name = name
        self.functions = {}
        self.classes = {}

    def label(self):
        return self.name + '_CompiledModule'

    def num_top_level_entries(self):
        return len(self.functions) + len(self.classes)

    def new_function(self, name):
        fn = CompiledFunction(self, name)
        self.functions[name] = fn
        return fn

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

    def name_ptr_for(self, name, names_section):
        acc = 0
        for entry_name_t, bsize in names_section:
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
               'object_table': [],
               'external_symbols': [],
               'names': [],
               'reloc_table': []
            }


        vmem.set_base(self.HEADER_SIZE)
        self.module.fill(vmem)

        mmc['header']['magic_number'] = self.MAGIC_NUMBER

        mmc['object_table'] = vmem.object_table()

        mmc['header']['ot_size'] = len(mmc['object_table'])

        external_symbols = vmem.external_symbols()
        strings_in_names = sorted(set([x[0] for x in external_symbols]))

        mmc['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in [name + '\0' for name in strings_in_names]]

        mmc['header']['names_size'] = sum([x[1] for x in mmc['names']])

        for pair in external_symbols:
            mmc['external_symbols'].append(self.name_ptr_for(pair[0], mmc['names']))
            mmc['external_symbols'].append(pair[1])

        mmc['header']['es_size'] = len(mmc['external_symbols']) * 2 * bits.WSIZE

        mmc['reloc_table'] = vmem.reloc_table()

        return mmc

    def dump(self, vmem):
        mmc = self.create_mmc_struct(vmem)

        with open(self.module.name + ".mmc", "w") as fp:
            # header
            fp.write(struct.pack('I', mmc['header']['magic_number']))
            fp.write(struct.pack('I', mmc['header']['ot_size']))
            fp.write(struct.pack('I', mmc['header']['es_size']))
            fp.write(struct.pack('I', mmc['header']['names_size']))

            # object table
            for v8 in mmc['object_table']:
                fp.write(struct.pack('B', v8))

            # external symbols
            for ptr in mmc['external_symbols']:
                fp.write(struct.pack('I', ptr))

            # names
            for name, chunk_size in mmc['names']:
                text = name + ((chunk_size - len(name)) * '\0')
                fp.write(text)

            # reloc table
            for v32 in mmc['reloc_table']:
                fp.write(struct.pack('I', v32))


class Compiler(ASTBuilder):
    def __init__(self):
        self.cmodule = None
        self.filepath = None

    def do_parse(self, parser):
        try:
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
