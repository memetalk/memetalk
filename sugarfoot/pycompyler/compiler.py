from pyparsers.parser import MemeParser
from pyparsers.memetr import MemeTr
from pyparsers.astbuilder import *
from pprint import pprint as P
import traceback
from pdb import set_trace as br
import os
import sys
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

    def __init__(self, module):
        self.module = module

    def fill(self, vmem):
        # header
        vmem.append_int(self.MAGIC_NUMBER)
        vmem.append_int(self.module.num_top_level_entries())
        vmem.append_label_ref(self.module.label())

        # -- obj table --
        self.module.fill(vmem)

        # -- ext refs --
        # -- names --
        # -- relloc table --
        vmem.dump()
        print vmem.external_references()


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
        mmc.fill(vmem)

    def new_module(self):
        module_name =  os.path.splitext(os.path.basename(self.filepath))[0]
        self.cmodule = CompiledModule(module_name)
        return self.cmodule

Compiler().compile(sys.argv[1])
