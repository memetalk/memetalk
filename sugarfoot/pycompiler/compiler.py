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

        labels = self.cmodule.entry_labels()

        names_list = set([l + "\0" for l in labels] + [n + "\0" for n in vmem.external_names()])
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

    def dump(self):
        vmem = comp_vmemory.CompVirtualMemory()
        mmc = self.create_mmc_struct(vmem)
        with open(self.cmodule.name + ".mmc", "w") as fp:
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


class Compiler(ASTBuilder):
    def __init__(self):
        self.cmodule = None
        self.filepath = None

    def do_parse(self, parser):
        try:
            parser.uses_literal = pyutils.Flag() # pymeta uses eval() which disables assignment. This works around it
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

        mmc = MMC(self.cmodule)
        mmc.dump()

    def new_module(self):
        module_name =  os.path.splitext(os.path.basename(self.filepath))[0]
        self.cmodule = entries.CompiledModule(module_name)
        return self.cmodule

Compiler().compile(sys.argv[1])
