# -*- coding: utf-8 -*-
# Copyright (c) 2012-2013 Thiago B. L. Silva <thiago@metareload.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

from parser import MemeParser
from coretr import CoreTr
from pprint import pprint, pformat
from pdb import set_trace as br
import traceback
import os
from config import MODULES_PATH
from astbuilder import *

def P(obj, depth=1):
    if depth > 5:
        depth = None
    pprint(obj, None, 1, 80, depth)

def to_source(x):
    return pformat(x)

# use a __repr__ that allows us to reconstruct the ast instance
def astnode_repr(self):
    return "ASTNode(" + str(self.lst) + "," + self.text.__repr__() + "," + \
        str(self.start_line) + "," + str(self.start_col) + "," + str(self.end_line) + ","+ str(self.end_col) + ")"
ASTNode.__repr__ = astnode_repr

class CoreGenerator(ASTBuilder):
    def __init__(self):
        self.parser = None
        self.line_offset = 0
        self.filename = 'core.md'

        self.objects = {}
        self.classes = {}
        self.current = None
        self.methods = {}
        self.funs = {}
        self.supers = {}
        self.fields = {}
        self.bv_methods = {}
        self.module_funs = {}

    def register_object(self, name):
        self.current = name
        self.objects[name] = {}
        self.funs[name] = {}

    def register_class(self, name, parent):
        self.current = name
        self.classes[name] = {'_vt':name+'Behavior',
                              'parent': 'Object'}
        self.methods[name] = {}
        #self.methods[name+"Behavior"] = {}
        self.bv_methods[name] = {}
        self.supers[name] = parent

    def to_source(self, x):
        return to_source(x)

    # def global_for(self, x):
    #     return "globals()["+to_source(x)+"]"

    def gen(self):

        self.parser = MemeParser(open(os.path.join(MODULES_PATH,self.filename)).read())
        self.parser.i = self
        try:
            ast = self.parser.apply("start")[0]
        except Exception as err:
            if hasattr(err, 'formatError'):
                print(err.formatError(''.join(self.parser.input.data)))
            else:
                traceback.print_exc()
            exit(1)

        self.run_tr(ast, 'start')
        self.dump()

    def dump(self):
        self.source = ''

        def append(x, indsize = 3):
            indent = ' ' * indsize
            self.source = self.source + indent + x + "\n"

        append("from astbuilder import *",0)
        append("def init(i, imod):",0)
        append("kernel_cmodule = i.new_object()")
        append("kernel_imodule = imod")

        for k,v in self.objects.iteritems():
            append(k + " = i.new_object()")

        for k,v in self.classes.iteritems():
            append(k + " = i.new_object()")
            append(k + "['@tag'] = '" + k + " class'")
            append(k+"Behavior = i.new_object({'_vt': Behavior,'parent':"+self.supers[k]+"Behavior, 'dict':{}, '@tag':'"+k+"Behavior'})")
            append(k+"Behavior" + "['@tag'] = '" + k + " behavior'")

        for name, val in self.classes.iteritems():
            append("kernel_imodule["+to_source(name)+"] = " + name)
        for name, val in self.objects.iteritems():
            append("kernel_imodule["+to_source(name)+"] = " + name)

        for name,obj in self.objects.iteritems():
            for key,v in obj.iteritems():
                append(name+"["+to_source(key)+"] = " + v)
            append(name+"['@tag'] = " + to_source(name))

        for name, dc in self.funs.iteritems():
            if len(dc) > 0: append(name+"['dict'] = {}")
            for mname,v in dc.iteritems():
                append(name+"['dict']["+to_source(mname)+"] = " + v)

        # for name, dc in self.methods.iteritems():
        #     if len(dc) > 0: print name+"['dict'] = {}"
        #     for mname,v in dc.iteritems():
        #         print name+"['dict']["+to_source(mname)+"] = " + v

        for name,obj in self.classes.iteritems():
            for key,v in obj.iteritems():
                append(name+"["+to_source(key)+"] = " + v)


        for name,obj in self.classes.iteritems():
            append(name + '["compiled_class"] = i.create_compiled_class({"_vt": CompiledClass,'+\
                       '"_delegate": None,'+\
                       '"name": "'+name+'",'+\
                       '"super_class_name":"'+self.supers[name]+'",'+\
                       '"fields": '+to_source(self.fields[name])+','+\
                       '"module": kernel_cmodule,'+\
                       '"methods": {},'+\
                       '"own_methods":{}})')


        for name, dc in self.methods.iteritems():
            if len(dc) > 0: append(name+"['dict'] = {}")
            for mname,v in dc.iteritems():
                append(name+"['compiled_class']['methods']["+to_source(mname)+"] = " + v)
                append(name+"['dict']["+to_source(mname)+"] = i.function_from_cfunction(" + name+"['compiled_class']['methods']["+to_source(mname)+"], kernel_imodule)")


        for name, dc in self.bv_methods.iteritems():
            for mname,v in dc.iteritems():
                append(name+"['compiled_class']['own_methods']["+to_source(mname)+"] = " + v)
                append(name+"Behavior['dict']["+to_source(mname)+"] = i.function_from_cfunction(" + name+"['compiled_class']['own_methods']["+to_source(mname)+"], kernel_imodule)")

        append("for name, m, in Object['dict'].iteritems():")
        append( "   Object_CompiledClass['methods'][name] = m['compiled_function']")
        append( "for name, m, in ObjectBehavior['dict'].iteritems():")
        append( "   Object_CompiledClass['own_methods'][name] = m['compiled_function']")


        append("kernel_cmodule.update(i.create_compiled_module({'name': 'core'," +\
                   "'ast': None," +\
                   "'parent_module':None," +\
                   "'@tag':'kernel compiled module'}))")

        append('KernelModule = i.new_object({"_vt": ModuleBehavior,')
        append('"_delegate": None,')
        append('"parent": Object,')
        append('"dict": {},')
        append('"@tag": "KernelModule"})')
        append("kernel_imodule['_vt'] = KernelModule")
        append("kernel_imodule['_delegate'] = None")
        append("kernel_imodule['@tag'] = 'Kernel module instance'")
        append("KernelModule['compiled_module'] = kernel_cmodule")

        for k,v in self.module_funs.iteritems():
            append(k + " = " + v)

        for name, val in self.module_funs.iteritems():
            append("kernel_imodule["+to_source(name)+"] = " + name)


        f = open("core_module.py", 'w')
        f.write(self.source)
        f.close()

    def run_tr(self, ast, prod):
        self.parser = CoreTr([ast])
        self.parser.i = self
        try:
            self.parser.apply(prod)
        except Exception as err:
            if hasattr(err, 'formatError'):
                P(err)
            else:
                traceback.print_exc()
            exit(1)

    def add_module_function(self, name, params, body):
        self.module_funs[name] = "i.function_from_cfunction("+\
            "i.create_compiled_function({"+\
                "'name': "+to_source(name)+","+\
                "'params': "+to_source(params)+","+\
                "'body': "+to_source(body)+","+\
                "'owner': kernel_cmodule,"+\
                "'@tag': '"+name+" compiled function'}),kernel_imodule)"

    def add_fun(self, name, params, body, is_ctor):
        self.funs[self.current][name] = "i.function_from_cfunction("+\
            "i.create_compiled_function({"+\
                "'name': "+to_source(name)+","+\
                "'params': "+to_source(params)+","+\
                "'body': "+to_source(body)+","+\
                "'is_ctor': "+to_source(is_ctor)+","+\
                "'@tag': '<"+self.current+">."+name+" compiled function'}),kernel_imodule)"

    def add_slot(self, name, value):
        self.objects[self.current][name] = str(value)

    def add_class_ctor(self, name, params, body):
        self.bv_methods[self.current][name] = "i.create_compiled_function({"+\
                "'name': "+to_source(name)+","+\
                "'params': "+to_source(params)+","+\
                "'body': "+to_source(body)+","+\
                "'is_ctor': "+to_source(True)+","+\
                "'owner': "+self.current+"['compiled_class'],"+\
                "'@tag': '<"+self.current+"Behavior>."+name+" compiled function'})"

    def add_class_method(self, name, params, body):
        self.methods[self.current][name] = "i.create_compiled_function({"+\
                "'name': "+to_source(name)+","+\
                "'params': "+to_source(params)+","+\
                "'body': "+to_source(body)+","+\
                "'is_ctor': "+to_source(False)+","+\
                "'owner': "+self.current+"['compiled_class'],"+\
                "'@tag': '<"+self.current+">."+name+" compiled function'})"

    def add_class_self_method(self, name, params,body):
        self.bv_methods[self.current][name] = "i.create_compiled_function({"+\
                "'name': "+to_source(name)+","+\
                "'params': "+to_source(params)+","+\
                "'body': "+to_source(body)+","+\
                "'is_ctor': "+to_source(False)+","+\
                "'owner': "+self.current+"['compiled_class'],"+\
                "'@tag': '<"+self.current+"Behavior>."+name+" compiled function'})"

    def add_class_fields(self, f):
        self.fields[self.current] = f

CoreGenerator().gen()
