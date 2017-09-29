from pyparsers.parser import MemeParser
from pyparsers.memetr import MemeTr
from pyparsers.astbuilder import *
import pyutils
from pyutils import bits
from pyutils import entries
import traceback
import os
import sys
import ejson
from . import comp_vmemory
import time

def bench(name, fn):
    print '*bench begin:', name
    b = time.time()
    ret = fn()
    print '*bench end:', name, ': ', time.time()-b
    return ret

class MEC(object):
    MAGIC_NUMBER = 0x420

    def __init__(self, cmodule):
        self.cmodule = cmodule

    def name_ptr_for(self, name, mec):
        acc = 0
        for entry_name_t, bsize in mec['names']:
            if entry_name_t[0:-1] == name:
                return self.HEADER_SIZE + mec['header']['ot_size'] + acc
            acc += bsize
        raise Exception('entry {} not found in NAMES'.format(name))

    def create_mec_struct(self, vmem):
        mec = {'header':
               {'magic_number': None,
                'ot_size': None,
                'er_size': None,
                'st_size': None,
                'names_size': None},
               'object_table': [],         # the loaded objects of this module
               'names': [],
               'external_references': [],  # references to core objects
               'symbol_table': [],         # Symbol objects needed
               'reloc_table': []           # addresses within the OT that need relocation on load
            }


        self.HEADER_SIZE = len(mec['header']) * bits.WSIZE
        #base = self.HEADER_SIZE + mec['header']['names_size']
        vmem.set_base(self.HEADER_SIZE)
        bench("cmodule.fill", lambda: self.cmodule.fill(vmem))

        mec['header']['magic_number'] = self.MAGIC_NUMBER

        # mec['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in [name + '\0' for name in vmem.external_names()]]

        # labels = self.cmodule.entry_labels()
        names_list = [n + "\0" for n in vmem.external_names()]

        mec['object_table'] = vmem.object_table()

        mec['header']['ot_size'] = len(mec['object_table'])

        mec['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in names_list]
        mec['header']['names_size'] = sum([x[1] for x in mec['names']])

        ext_ref = vmem.external_references()
        for pair in ext_ref:
            mec['external_references'].append(self.name_ptr_for(pair[0], mec))
            mec['external_references'].append(pair[1])

        mec['header']['er_size'] = len(mec['external_references']) * bits.WSIZE

        sym_ref = vmem.symbols_references()
        for pair in sym_ref:
            mec['symbol_table'].append(self.name_ptr_for(pair[0], mec))
            mec['symbol_table'].append(pair[1])

        mec['header']['st_size'] = len(mec['symbol_table']) * bits.WSIZE

        mec['reloc_table'] = vmem.reloc_table()

        return mec

    def dump(self, filepath):
        vmem = comp_vmemory.CompVirtualMemory()
        mec = self.create_mec_struct(vmem)
        with open(filepath[:-2] + "mec", "w") as fp:
            # header
            fp.write(bits.pack_word(mec['header']['magic_number']))
            fp.write(bits.pack_word(mec['header']['ot_size']))
            fp.write(bits.pack_word(mec['header']['er_size']))
            fp.write(bits.pack_word(mec['header']['st_size']))
            fp.write(bits.pack_word(mec['header']['names_size']))

            # object table
            for v in mec['object_table']:
                fp.write(bits.pack_byte(v))

            # names
            for name, chunk_size in mec['names']:
                text = name + ((chunk_size - len(name)) * '\0')
                fp.write(text)

            # external references
            for v in mec['external_references']:
                fp.write(bits.pack_word(v))

            # symbols
            for v in mec['symbol_table']:
                fp.write(bits.pack_word(v))

            # reloc table
            for word in mec['reloc_table']:
                fp.write(bits.pack_word(word))



class MEC_Fun(object):
    MAGIC_NUMBER = 0x420

    def __init__(self, cmod, cfun):
        self.cmod = cmod
        self.cfun = cfun

    def name_ptr_for(self, name, mec):
        acc = 0
        for entry_name_t, bsize in mec['names']:
            if entry_name_t[0:-1] == name:
                return self.HEADER_SIZE + acc
            acc += bsize
        raise Exception('entry {} not found in NAMES'.format(name))

    def create_mec_struct(self, vmem):
        mec = {'header':
               {'magic_number': None,
                'ot_size': None,
                'er_size': None,
                'st_size': None,
                'names_size': None,
                'cfun_addr': None},
               'names': [],
               'object_table': [],
               'external_references': [],
               'symbol_table': [],
               'reloc_table': []
            }


        self.HEADER_SIZE = len(mec['header']) * bits.WSIZE
        self.cmod.fill(vmem)

        mec['header']['magic_number'] = self.MAGIC_NUMBER

        names_list = set([n + "\0" for n in vmem.external_names()])

        mec['names'] = [(name_t, bits.string_block_size(name_t)) for name_t in names_list]
        mec['header']['names_size'] = sum([x[1] for x in mec['names']])

        base = self.HEADER_SIZE + mec['header']['names_size']
        vmem.set_base(base)

        mec['object_table'] = vmem.object_table()
        mec['header']['ot_size'] = len(mec['object_table'])

        for pair in vmem.external_references():
            mec['external_references'].append(self.name_ptr_for(pair[0], mec))
            mec['external_references'].append(pair[1])

        mec['header']['er_size'] = len(mec['external_references']) * bits.WSIZE

        for pair in vmem.symbols_references():
            mec['symbol_table'].append(self.name_ptr_for(pair[0], mec))
            mec['symbol_table'].append(pair[1])

        mec['header']['st_size'] = len(mec['symbol_table']) * bits.WSIZE

        mec['reloc_table'] = vmem.reloc_table()

        mec['header']['cfun_addr'] = vmem.physical_address(self.cfun.oop)
        return mec

    def dump(self):
        vmem = comp_vmemory.CompVirtualMemory()
        mec = self.create_mec_struct(vmem)

        fp = sys.stdout

        # header
        fp.write(bits.pack_word(mec['header']['magic_number']))
        fp.write(bits.pack_word(mec['header']['ot_size']))
        fp.write(bits.pack_word(mec['header']['er_size']))
        fp.write(bits.pack_word(mec['header']['st_size']))
        fp.write(bits.pack_word(mec['header']['names_size']))
        fp.write(bits.pack_word(mec['header']['cfun_addr']))

        # names
        for name, chunk_size in mec['names']:
            text = name + ((chunk_size - len(name)) * '\0')
            fp.write(text)

        # object table
        for v in mec['object_table']:
            fp.write(bits.pack_byte(v))

        # external references
        for v in mec['external_references']:
            fp.write(bits.pack_word(v))

        # symbols
        for v in mec['symbol_table']:
            fp.write(bits.pack_word(v))

        # reloc table
        for word in mec['reloc_table']:
            fp.write(bits.pack_word(word))



class Compiler(ASTBuilder):
    def __init__(self):
        self.cmodule = None
        self.filepath = None

    def new_module(self):
        module_name =  os.path.splitext(os.path.basename(self.filepath))[0]
        self.cmodule = entries.CompiledModule(module_name)
        return self.cmodule

    def do_parse(self, parser, rule, *args):
        try:
            parser.has_fun_literal = pyutils.Flag() # pymeta uses eval() which disables assignment. This works around it
            return parser.apply(rule, *args)
        except Exception as err:
            print '\n'
            if hasattr(err,'formatError') and isinstance(parser.input.data[0], (str, unicode)):
                print err.formatError(''.join(parser.input.data))
            elif hasattr(err,'formatError') :
                print err.formatError(parser.input.data)
            else:
                print err, traceback.format_exc()
            sys.exit(1)

    def compile(self, filepath):
        self.filepath = filepath
        self.line_offset = 0
        self.parser = MemeParser(open(filepath, 'r').read())
        self.parser.i = self

        ast = bench('parsing', lambda: self.do_parse(self.parser, 'start')[0])
        # print ast
        # print str(ast)[0:70] + "..."

        self.parser = MemeTr([ast])
        self.parser.i = self

        bench('translate', lambda: self.do_parse(self.parser, "start"))

        mec = MEC(self.cmodule)
        bench('mec dump', lambda: mec.dump(filepath))

    def compile_lambda(self, text, env_names):
        self.filepath = '<eval>'
        self.line_offset = 0

        self.parser = MemeParser(text)
        self.parser.i = self

        ast = self.do_parse(self.parser, 'cfunliteral_body')[0]
        # print ast

        cmod = entries.CompiledModule('dummy_module')
        cfun = cmod.new_function('<eval>', [])
        cfun.uses_env(True)
        cfun.declare_vars(env_names)

        self.parser = MemeTr([cfun, ast])
        self.parser.i = self

        self.do_parse(self.parser, 'cfunliteral')


        mec = MEC_Fun(cmod, cfun)
        mec.dump()

    def recompile_function(self, line, text):
        self.filepath = '<???>'
        self.line_offset = int(line)

        self.parser = MemeParser(text)
        self.parser.i = self

        ast = self.do_parse(self.parser, 'fun_rest', 'random_name')[0]
        # print ast

        # cmod = entries.CompiledModule('dummy_module')
        # cfun = cmod.new_function('<????????>', [])

        cmod = self.new_module()
        self.parser = MemeTr([cmod, ast])
        self.parser.i = self

        self.do_parse(self.parser, 'function_definition')

        cfun = cmod.functions['random_name']
        cfun.uses_env(False)
        mec = MEC_Fun(cmod, cfun)
        mec.dump()


def main(*paths):
    for path in paths:
        Compiler().compile(path)

def compile_lambda(js):
    # python -m pycompiler.compiler compile-lambda '{"text":"fun() {X.z}","env_names":""}'
    # python -m pycompiler.compiler compile-lambbd '{"text":"fun() {a}","env_names":["a"]}'
    js = ejson.loads(js)
    Compiler().compile_lambda(js['text'], js['env_names'])

def recompile_fun(js):
    # python -m pycompiler.compiler recompile-fun '{"text":"fun() {X.z}"}'
    js = ejson.loads(js)
    Compiler().recompile_function(js["start_line"], js['text'])

if __name__ == '__main__':
    from clime import Program
    prog = Program(debug=True, default='main')
    prog.main()
