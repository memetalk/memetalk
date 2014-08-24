from pyparsers.parser import MemeParser
from pyparsers.memetr import MemeTr
from pyparsers.astbuilder import *
import pyutils
from pyutils import bits
from pyutils import entries
from pprint import pprint as P
import traceback
from pdb import set_trace as br
import os
import sys
import getopt
import ejson
from . import comp_vmemory

class MMC(object):
    MAGIC_NUMBER = 0x420

    def __init__(self, cmodule):
        self.cmodule = cmodule

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
                'er_size': None,
                'es_size': None,
                'names_size': None},
               'names': [],
               'object_table': [],
               'external_references': [],
               'external_symbols': [],
               'exception_types': [],
               'reloc_table': []
            }


        self.HEADER_SIZE = len(mmc['header']) * bits.WSIZE
        self.cmodule.fill(vmem)

        mmc['header']['magic_number'] = self.MAGIC_NUMBER

        # mmc['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in [name + '\0' for name in vmem.external_names()]]

        # labels = self.cmodule.entry_labels()
        names_list = set([n + "\0" for n in vmem.external_names()])

        mmc['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in names_list]
        mmc['header']['names_size'] = sum([x[1] for x in mmc['names']])

        base = self.HEADER_SIZE + mmc['header']['names_size']
        vmem.set_base(base)

        mmc['object_table'] = vmem.object_table()
        mmc['header']['ot_size'] = len(mmc['object_table'])

        for pair in vmem.external_references():
            mmc['external_references'].append(self.name_ptr_for(pair[0], mmc))
            mmc['external_references'].append(pair[1])

        mmc['header']['er_size'] = len(mmc['external_references']) * bits.WSIZE

        for pair in vmem.symbols_references():
            mmc['external_symbols'].append(self.name_ptr_for(pair[0], mmc))
            mmc['external_symbols'].append(pair[1])

        mmc['header']['es_size'] = len(mmc['external_symbols']) * bits.WSIZE

        mmc['reloc_table'] = vmem.reloc_table()

        return mmc

    def dump(self, filepath):
        vmem = comp_vmemory.CompVirtualMemory()
        mmc = self.create_mmc_struct(vmem)
        with open(filepath[:-2] + "mmc", "w") as fp:
            # header
            fp.write(bits.pack_word(mmc['header']['magic_number']))
            fp.write(bits.pack_word(mmc['header']['ot_size']))
            fp.write(bits.pack_word(mmc['header']['er_size']))
            fp.write(bits.pack_word(mmc['header']['es_size']))
            fp.write(bits.pack_word(mmc['header']['names_size']))

            # names
            for name, chunk_size in mmc['names']:
                text = name + ((chunk_size - len(name)) * '\0')
                fp.write(text)

            # object table
            for v in mmc['object_table']:
                fp.write(bits.pack_byte(v))

            # external references
            for v in mmc['external_references']:
                fp.write(bits.pack_word(v))

            # external symbols
            for v in mmc['external_symbols']:
                fp.write(bits.pack_word(v))

            # reloc table
            for word in mmc['reloc_table']:
                fp.write(bits.pack_word(word))



class MMC_Fun(object):
    MAGIC_NUMBER = 0x420

    def __init__(self, cmod, cfun):
        self.cmod = cmod
        self.cfun = cfun

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
                'er_size': None,
                'es_size': None,
                'names_size': None,
                'cfun_addr': None},
               'names': [],
               'object_table': [],
               'external_references': [],
               'external_symbols': [],
               'exception_types': [],
               'reloc_table': []
            }


        self.HEADER_SIZE = len(mmc['header']) * bits.WSIZE
        self.cmod.fill(vmem)

        mmc['header']['magic_number'] = self.MAGIC_NUMBER

        names_list = set([n + "\0" for n in vmem.external_names()])

        mmc['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in names_list]
        mmc['header']['names_size'] = sum([x[1] for x in mmc['names']])

        base = self.HEADER_SIZE + mmc['header']['names_size']
        vmem.set_base(base)

        mmc['object_table'] = vmem.object_table()
        mmc['header']['ot_size'] = len(mmc['object_table'])

        for pair in vmem.external_references():
            mmc['external_references'].append(self.name_ptr_for(pair[0], mmc))
            mmc['external_references'].append(pair[1])

        mmc['header']['er_size'] = len(mmc['external_references']) * bits.WSIZE

        for pair in vmem.symbols_references():
            mmc['external_symbols'].append(self.name_ptr_for(pair[0], mmc))
            mmc['external_symbols'].append(pair[1])

        mmc['header']['es_size'] = len(mmc['external_symbols']) * bits.WSIZE

        mmc['reloc_table'] = vmem.reloc_table()

        mmc['header']['cfun_addr'] = vmem.physical_address(self.cfun.oop)
        return mmc

    def dump(self):
        vmem = comp_vmemory.CompVirtualMemory()
        mmc = self.create_mmc_struct(vmem)

        fp = sys.stdout

        # header
        fp.write(bits.pack_word(mmc['header']['magic_number']))
        fp.write(bits.pack_word(mmc['header']['ot_size']))
        fp.write(bits.pack_word(mmc['header']['er_size']))
        fp.write(bits.pack_word(mmc['header']['es_size']))
        fp.write(bits.pack_word(mmc['header']['names_size']))
        fp.write(bits.pack_word(mmc['header']['cfun_addr']))

        # names
        for name, chunk_size in mmc['names']:
            text = name + ((chunk_size - len(name)) * '\0')
            fp.write(text)

        # object table
        for v in mmc['object_table']:
            fp.write(bits.pack_byte(v))

        # external references
        for v in mmc['external_references']:
            fp.write(bits.pack_word(v))

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

    def new_module(self):
        module_name =  os.path.splitext(os.path.basename(self.filepath))[0]
        self.cmodule = entries.CompiledModule(module_name)
        return self.cmodule

    def do_parse(self, parser, rule):
        try:
            parser.has_fun_literal = pyutils.Flag() # pymeta uses eval() which disables assignment. This works around it
            return parser.apply(rule)
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

        ast = self.do_parse(self.parser, 'start')[0]

        print ast

        self.parser = MemeTr([ast])
        self.parser.i = self

        self.do_parse(self.parser, "start")

        mmc = MMC(self.cmodule)
        mmc.dump(filepath)

    def compile_function(self, text, env_names):
        self.filepath = '<eval>'
        self.line_offset = 0

        self.parser = MemeParser(text)
        self.parser.i = self

        ast = self.do_parse(self.parser, 'funliteral')[0]

        # print ast

        cmod = entries.CompiledModule('dummy_module')
        cfun = cmod.new_function('<eval>', [])
        cfun.uses_env(True)
        cfun.declare_vars(env_names)

        self.parser = MemeTr([cfun, ast])
        self.parser.i = self

        self.do_parse(self.parser, 'cfunliteral')


        mmc = MMC_Fun(cmod, cfun)
        mmc.dump()



opts, nonopts = getopt.getopt(sys.argv[1:], 'f:o:')
if len(opts) == 0:
    for path in sys.argv[1:]:
        Compiler().compile(path)
else:
    # python -m pycompiler.compiler -o '{"text":"fun() {X.z}","env_names":""}'
    # python -m pycompiler.compiler -o '{"text":"fun() {a}","env_names":["a"]}'

    dopts = dict(opts)
    js = ejson.loads(dopts['-o'])
    Compiler().compile_function(js['text'], js['env_names'])
