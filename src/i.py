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

from greenlet import greenlet
import parser
import sys
import os
from parser import MemeParser
from loader import Loader
from evaluator import Eval
from prim import *
import core_module as core
from pprint import pprint, pformat
from pdb import set_trace as br
import traceback
from config import MODULES_PATH
from astbuilder import *

def P(obj, depth=1):
    if depth > 5:
        depth = None
    pprint(obj, None, 1, 80, depth)


def _should_dump_mast():
    return 'DEBUG' in os.environ and os.environ['DEBUG'] == 'full'

def _should_dump_ast():
    return 'DEBUG' in os.environ

def _should_warn_of_exception():
    return 'DEBUG' in os.environ


def _create_compiled_module(data):
    template = {"_vt": core.CompiledModule,
                "_delegate": None,
                "name": "",
                "filepath":"",
                "params": [],
                "default_params": [],
                "aliases": [],
                "compiled_functions": {},
                "compiled_classes": {},
                "@tag":" a CompiledModule"}

    return dict(template.items() + data.items())

def _create_compiled_function(data):
    template = {"_vt": core.CompiledFunction,
                "_delegate": None,
                "name": "",
                "params": [],
                "body": None,
                "line":0,
                "text": '/* native */',
                "is_prim": False, # not currently in use
                "prim_name": '',  # not currently in use
                "uses_env": False,
                "fun_literals":{},
                "env_table":{},
                "env_table_skel": {},
                "outer_cfun":None,
                "owner":None,
                "is_ctor": False,
                "is_top_level": True,
                "is_embedded": True,
                "@tag": "a CompiledFunction"}
    return dict(template.items() + data.items())

def _create_compiled_class(data):
    template = {"_vt": core.CompiledClass,
                "_delegate": None,
                "module": None, #compiled module
                "name": "",
                "super_class_name":"Object",
                "fields": [],
                "methods": {},
                "own_methods":{},
                "@tag":"a CompiledClass"}
    return dict(template.items() + data.items())

def _create_module(data):
    template = {"_vt": core.ModuleBehavior,
                "_delegate": None,
                "parent": core.Object,
                "size": 1, #delegate
                "dict": {},
                "compiled_module": None,
                "@tag": "a Module"}
    return dict(template.items() + data.items())

def _create_class(data):
    template = {"_vt": None, #it should be given by data [FooClassBehavior]
                "_delegate": None,
                "parent": None,
                "size": 1, #delegate
                "dict": {},
                "compiled_class":None,
                "@tag":"a class"}
    return dict(template.items() + data.items())


def _create_accessor_method(imodule, name):
    cf =  _create_compiled_function({
            "name": name,
            "body":  [ASTNode(['return', ['field', name]],'return @'+name+';',0,0,0,0)]})
    return _function_from_cfunction(cf, imodule)

def _function_from_cfunction(cfun, imodule):
    return {"_vt": core.Function,
            "_delegate": None,
            "compiled_function": cfun,
            "module": imodule,
            "@tag": "a Function"}

def _compiled_function_to_context(cfun, env, imodule):
    return {"_vt": core.Context,
            "_delegate": None,
            "compiled_function": cfun,
            "env":env,
            "module": imodule,
            "@tag": "a Context"}

def _compiled_functions_to_functions(cfuns, imodule):
    funs = {}
    for name, fun in cfuns.items():
        funs[name] = _function_from_cfunction(fun, imodule)
    return funs

def _create_instance(klass, data):
    template = {"_vt": klass,
                "_delegate": None}
    return dict(template.items() + data.items())


def _instantiate_module(i, compiled_module, _args, parent_module):
    def setup_module_arguments(_args, compiled_module):
        args = dict(_args)
        # module's default argument
        for mp in compiled_module['default_params']:
            if mp[1] not in args.keys():
                if mp[0] == 'lib':
                    args[mp[1]] = i.compile_module_lib(mp[2])
                elif mp[0] == 'uri':
                    args[mp[1]] = i.compile_module_uri(mp[2])
                else:
                    i.throw_with_value('Unknown module spec')
        if args.keys().sort() != compiled_module['params'].sort():
            i.throw_with_value('arity error on module parameters')

        # module's aliases. dirty: using arg to set it
        for alias in compiled_module['aliases']:
            for name in alias[1]:
                args[name] = args[alias[0]][name]
        return args

    args = setup_module_arguments(_args, compiled_module)

    #creates the Module object and its instance
    size = len(compiled_module["params"])+\
        len(compiled_module["compiled_classes"])+\
        len(compiled_module["compiled_functions"])

    # puff
    imod_dictionary = {"_compiledModule":_function_from_cfunction(
            _create_compiled_function({
                    'name':'_compiledModule',
                    'params':[],
                    'body': ['primitive', ['literal-string', 'module_instance_compiled_module']],
                    '@tag': '<Module>._compiledModule compiled function'}),
            core.kernel_imodule)}

    # Module
    module = _create_module({"_vt": core.ModuleBehavior,
                             "size": size+1, #+1 delegate
                             "dict": imod_dictionary,
                             "compiled_module": compiled_module,
                             "@tag":"Module " + compiled_module["name"]})

    # module instance
    imodule = {"_vt": module,
               "_delegate": parent_module,
               "@tag":"Module instance: " + compiled_module["name"]}

    # accessors
    for cname, cclass in compiled_module["compiled_classes"].items():
        imod_dictionary[cname] = _create_accessor_method(imodule, cname)

    # functions as module methods
    for cfname, cfun in compiled_module["compiled_functions"].items():
        imod_dictionary[cfname] = _function_from_cfunction(cfun,imodule)

    #instantiate classes

    def lookup_super_class_on_parent_module(super_name, module):
        if super_name in module:
            return module[super_name] # TODO: should be actually a msg send
        elif module["_delegate"]:
            return lookup_super_class_on_parent_module(super_name, module['_delegate'])

    classes = {}
    bclasses = {}
    super_later = {}
    for _, c in compiled_module["compiled_classes"].items():
        super_name = c["super_class_name"]
        super_class = None
        if super_name in args.keys():
            super_class = args[super_name]
        elif lookup_super_class_on_parent_module(super_name, parent_module):
            super_class = lookup_super_class_on_parent_module(super_name, parent_module)
        elif super_name in compiled_module["compiled_classes"]:
            super_later[c["name"]] = super_name
        else:
            i.throw_with_value("super class not found: " + super_name)

        # superclass BarClass found
        if super_class:
            # FooClassBehavior
            cbehavior = {"_vt": core.Behavior,
                         "parent": super_class["_vt"],
                         "dict": _compiled_functions_to_functions(c["own_methods"], imodule),
                         "@tag":c["name"]+" Behavior"}

            classes[c["name"]] = _create_class({"_vt": cbehavior,
                                                "parent": super_class,
                                                "size": len(c["fields"])+1,
                                                "dict": _compiled_functions_to_functions(c["methods"],imodule),
                                                "compiled_class":c,
                                                "@tag":c["name"]+" Class"})
            bclasses[c["name"]] = cbehavior
        else:
            # FooClassBehavior
            cbehavior = {"_vt": core.Behavior,
                         "parent": "*replace-me*", #latter below
                         "dict": _compiled_functions_to_functions(c["own_methods"],imodule),
                         "@tag":c["name"]+" Behavior"}

            bclasses[c["name"]] = cbehavior
            # superclass BarClass will be eventually in the variable 'classes'
            classes[c["name"]] = _create_class({"_vt": cbehavior,
                                                "parent": "*replace-me*", #placeholder to substitute later below
                                                "size": len(c["fields"])+1,
                                                "dict": _compiled_functions_to_functions(c["methods"],imodule),
                                                "compiled_class":c,
                                                "@tag":c["name"]+" Class"})

    for name, super_name in super_later.items():
        classes[name]["parent"] = classes[super_name]
        classes[name]["_vt"]["parent"] = bclasses[super_name]

    # #update parent classes
    # for name, c in classes.items():
    #     parent_name = classes[name]["parent"]

    funs = _compiled_functions_to_functions(compiled_module["compiled_functions"], imodule)

    for name, fun in funs.items():
        imodule[name] = fun
    for name, klass in classes.items():
        imodule[name] = klass

    for name, val in args.iteritems():
        imodule[name] = val

    return imodule

#######################################################
## Loading
#######################################################


class ModuleLoader(ASTBuilder):
    def __init__(self):
        self.first_fnlit = None
        self.recompiling_cfun = None

    def recompile_top_level(self, i, cfun, src):
        self.line_offset = 0
        self.pos_stack = []
        name = cfun['name']

        self.recompiling_cfun = cfun

        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("single_top_level_fun", name)
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())

        if _should_dump_ast():
            print "---- AST ----"
            print ast
            print "//---- AST ----"

        self.env_id_table = []
        self.env_idx = 0
        self.fun_literals = []

        self.functions = [self.recompiling_cfun]

        loader = Loader([ast])
        loader.i = self

        try:
            owner = cfun['owner']
            if owner['_vt'] == core.CompiledClass:
                self.current_class = owner
                self.current_module = owner['module']
                if cfun in owner['methods'].values():
                    loader.apply("function_definition", "instance_method")
                elif cfun['is_ctor']:
                    loader.apply("function_definition", "constructor")
                else:
                    loader.apply("function_definition", "class_method")
                return cfun
            else:
                self.current_module = cfun['owner']
                loader.apply("function_definition", "module_function")
                return cfun
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())


    def recompile_closure(self, i, cfun, src):
        self.line_offset = 0
        self.pos_stack = []

        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("funliteral")
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())

        if _should_dump_ast():
            print "---- AST ----"
            print ast
            print "//---- AST ----"

        self.env_id_table = []
        self.env_idx = 0
        self.fun_literals = []

        self.functions = [cfun['outer_cfun'],cfun]

        self.first_fnlit = cfun
        loader = Loader([ast])
        loader.i = self
        try:
            loader.apply("load_fun_lit")
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())
        return cfun

    def compile_closure(self, i, src, outer):
        self.line_offset = 0
        self.pos_stack = []

        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("funliteral")
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())

        if _should_dump_ast():
            print "---- AST ----"
            print ast
            print "//---- AST ----"

        self.env_id_table = []
        self.env_idx = 0
        self.fun_literals = []

        self.functions = [outer]

        self.first_fnlit = None
        loader = Loader([ast])
        loader.i = self
        try:
            loader.apply("load_fun_lit")
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())

        return self.first_fnlit

    def compile_top_level(self, i, name, src, owner, flag):
        self.line_offset = 0
        self.pos_stack = []

        if owner['_vt'] == core.CompiledClass:
            self.current_class = owner
            self.current_module = owner["module"]
        else:
            self.current_module = owner

        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("single_top_level_fun", name)
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())

        if _should_dump_ast():
            print "---- AST ----"
            print ast
            print "//---- AST ----"

        self.env_id_table = []
        self.env_idx = 0
        self.fun_literals = []

        self.functions = []

        loader = Loader([ast])
        loader.i = self
        try:
            loader.apply("function_definition", flag['self'])
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())

        if flag['self'] == "class_method" or flag['self'] == "constructor":
            return self.current_class["own_methods"][name]
        elif flag['self'] == 'instance_method':
            return self.current_class["methods"][name]
        else:
            return self.current_module["compiled_functions"][name]

    def compile_module(self, i, filename, src):
        self.line_offset = 0
        self.pos_stack = []
        #self.function_line_offset = 0

        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("start")
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())

        if _should_dump_mast():
            print "---- AST ----"
            print ast
            print "//---- AST ----"

        self.current_module = _create_compiled_module({"filepath": filename,
                                                       "ast": ast,
                                                       "parent_module":"memetalk/kernel",
                                                       "@tag":"a compiled module"})

        self.env_id_table = []
        self.env_idx = 0
        self.functions = []
        self.fun_literals = []

        loader = Loader([ast])
        loader.i = self
        try:
            loader.apply("load_module")
        except Exception as err:
            if hasattr(err,'formatError'):
                i.throw_with_value(err.formatError(''.join(self.parser.input.data)))
            else:
                i.throw_with_value(traceback.format_exc())

        return self.current_module

    def l_module(self, name, p):
        self.current_module["name"] = name
        self.current_module["params"] = p

    def l_default_p_lib(self, name, spec, args):
        self.current_module['default_params'].append(("lib", name, spec, args))

    def l_default_p_uri(self, name, uri, args):
        self.current_module['default_params'].append(("uri", name, uri, args))

    def l_module_alias(self, libname, aliases):
        self.current_module['aliases'].append((libname, aliases))

    def l_begin_class(self, name, super_class, fields):
        self.current_class = _create_compiled_class({"name":name,
                                                     "super_class_name":super_class,
                                                     "module": self.current_module,
                                                     "fields":fields})

    def l_end_class(self):
        cname = self.current_class["name"]
        self.current_module["compiled_classes"][cname] = self.current_class
        self.current_class = None

    def l_begin_function(self, tp, name):
        self.env_id_table.append({})

        if tp == "instance_method" or tp == "class_method" or tp == "constructor":
            owner = self.current_class
        else:
            owner = self.current_module

        if self.recompiling_cfun:
            self.recompiling_cfun['name'] = name
            self.recompiling_cfun['is_ctor'] = tp == "constructor"
        else:
            self.functions.append(_create_compiled_function({"name":name,
                                                             "is_ctor": tp == "constructor",
                                                             'owner': owner,
                                                             "@tag":"a compiled function"}))

    def l_var_def(self, name):
        self.env_id_table[-1][self.env_idx] = name
        self.env_idx = self.env_idx + 1

    def l_set_function_parameters(self, params):
        self.env_idx = 0
        self.env_id_table[-1] = dict(zip(range(self.env_idx,self.env_idx+len(params)),params))
        self.env_idx = len(params)
        self.functions[-1]["params"] = params

    def l_end_function(self, tp, body, ast):
        function = self.functions.pop()
        function['body'] = body
        function['text'] = ast.text
        function['line'] = ast.start_line

        env_table = self.env_id_table.pop()
        if function["uses_env"]:
            function['env_table'] = env_table
            function['env_table_skel'] =  dict(zip(range(0,self.env_idx),[None]*self.env_idx))

        fname = function["name"]
        if tp == "class_method" or tp == "constructor":
            self.current_class["own_methods"][fname] = function
        elif tp == 'instance_method':
            self.current_class["methods"][fname] = function
        else:
            self.current_module["compiled_functions"][fname] = function


    def l_enter_literal_fun(self):
        self.env_id_table.append({})
        self.functions[0]["uses_env"] = True # root Function using env
        cfun = _create_compiled_function({"name":"<anonymous>",
                                          "outer_cfun": self.functions[-1],
                                          "owner": self.functions[-1]['owner'],
                                          "is_top_level": False,
                                          "@tag":"a compiled literal function"})
        self.functions.append(cfun)

    def l_enter_first_literal_fun(self):
        self.env_id_table.append({})
        if self.first_fnlit == None:
            self.first_fnlit = _create_compiled_function({"name":"<anonymous>",
                                                          "outer_cfun": self.functions[-1],
                                                          "owner": self.functions[-1]['owner'],
                                                          "is_top_level": False,
                                                          "@tag":"a compiled literal function"})
            self.functions.append(self.first_fnlit)

    def l_set_fun_literal_parameters(self, params):
        self.env_id_table[-1] = dict(zip(range(self.env_idx,self.env_idx+len(params)),params))
        self.env_idx = self.env_idx + len(params)

        self.functions[-1]["params"] = params

    def l_literal_fun_body(self, body):
        self.functions[-1]["body"] = body

    def l_done_literal_function(self, ast):
        function = self.functions.pop()

        body = function["body"]
        function['text'] = ast.text
        function['line'] = ast.start_line

        function['env_table'] = self.env_id_table.pop()

        #hack to identify the literal function when executing it
        #body[0]: the first instruction. The body changes id, unfortunately
        #literal is in the parent fun, so it can be easily fetched during execution
        self.functions[-1]["fun_literals"][id(body[0])] = function


#######################################################
## Executing
#######################################################

class VMException(Exception):
    pass

class ReturnException(VMException):
    def __init__(self, val):
        self.val = val

class RewindException(VMException):
    def __init__(self, count):
        self.count = count

class MemetalkException(VMException):
    def __init__(self, mmobj):
        self._mmobj = mmobj
    def mmobj(self):
        return self._mmobj

class Interpreter():
    def __init__(self):
        self.compiled_modules = {}
        self.processes = []
        self.current_process = None
        self.mem_imodules = [core.kernel_imodule]
        self.interned_symbols = {}

    def open_module_file(self, filename):
        if os.path.isfile(filename):
            return open(filename)
        else:
            return open(os.path.join(MODULES_PATH, filename))

    # def load_modules(self):
    #     sources = [f[:-3] for f in os.listdir(MODULES_PATH) \
    #                    if os.path.isfile(os.path.join(MODULES_PATH,f)) and \
    #                    f != 'core.md']

    #     for name in sources:
    #         cm = self.compile_module(name)
    #         self.compiled_modules[cm['name']] = cm #TODO: should be msg send
    #     br()
    #     self.compiled_modules[core.kernel_imodule['name']] = core.kernel_imodule

    def start(self, filename):
        #self.load_modules()
        compiled_module = self.compiled_module_by_filename(filename)

        imodule = self.instantiate_module(compiled_module, {}, core.kernel_imodule)

        self.current_process = Process(self)
        self.processes.append(self.current_process)
        ret = self.current_process.switch('run_module', 'main', imodule, [])
        print "RETVAL: " + pformat(ret,1,80,2)

    def debug_process(self, target_process):
        target_process.state = 'paused'

        compiled_module = self.compiled_module_by_filename('idez.mm')
        imodule = self.instantiate_module(compiled_module, {}, core.kernel_imodule)

        target_process.debugger_process = Process(self)
        self.processes.append(target_process.debugger_process)

        mmprocess = self.alloc(core.VMProcess, {'self':target_process})
        ret = target_process.debugger_process.switch('run_module', 'debug', imodule, [mmprocess])
        print('debug_process: switch back from debugger_process')
        return ret

    def compiled_module_by_filename(self, filename):
        module_name = filename[:-3]
        if module_name not in self.compiled_modules:
            source = self.open_module_file(filename).read()
            self.compiled_modules[module_name] =  ModuleLoader().compile_module(self,filename,source)
        return self.compiled_modules[module_name]

    def compiled_module_by_filepath(self, filepath):
        module_name = filepath
        if module_name not in self.compiled_modules:
            source = open(filepath).read()
            self.compiled_modules[module_name] =  ModuleLoader().compile_module(self,filepath,source)
        return self.compiled_modules[module_name]

    def instantiate_module(self, compiled_module, args, imodule):
        imodule = _instantiate_module(self, compiled_module, args, imodule)
        self.mem_imodules.append(imodule)
        return imodule

    def compile_module_lib(self, spec):
        name = spec[1]
        compiled_module = self.compiled_module_by_filename(name + '.mm')
        return self.instantiate_module(compiled_module, {}, core.kernel_imodule)

    def compile_module_uri(self, uri):
        self.throw_with_value('TODO: compile_module_uri')

    def kernel_module_instance(self):
        return core.kernel_imodule

    def get_core_class(self, name):
        return getattr(core, name)

    def throw(self, mex):
        # MemetalkException encapsulates the memetalk exception:
        raise MemetalkException(mex)

    def throw_with_value(self, value):
        if _should_warn_of_exception():
            print "MemetalkException with value: \n" + value
        ex = self.create_instance(core.Exception)

        # ...and this me being stupid:
        ex['value'] =  value

        # MemetalkException encapsulates the memetalk exception:
        raise MemetalkException(ex)

    def create_instance(self, klass):
        p = None
        if klass["parent"] != None:
            p = self.create_instance(klass["parent"])
        fields = klass["compiled_class"]["fields"]
        name = klass["compiled_class"]["name"]
        res = dict([(x,None) for x in fields] + {"_vt": klass, "_delegate":p, "@tag": name + " instance"}.items() )
        return res

    def create_compiled_function(self, data):
        return _create_compiled_function(data)

    def compiled_functions_to_functions(self, cfuns, imod):
        return _compiled_functions_to_functions(cfuns, imod)

    def create_compiled_class(self, data):
        return _create_compiled_class(data)

    def create_class(self, data):
        return _create_class(data)

    def create_function_from_cfunction(self, cfun, imodule):
        return _function_from_cfunction(cfun, imodule)

    def create_accessor_method(self, imodule, name):
        return _create_accessor_method(imodule, name)

    def compiled_function_to_context(self, cfun, env, imodule):
        return _compiled_function_to_context(cfun, env, imodule)

    def compile_top_level(self, name,text, owner, flags):
        return ModuleLoader().compile_top_level(self, name, text, owner, flags)

    def compile_closure(self, text, cfun):
        return ModuleLoader().compile_closure(self, text, cfun)

    def recompile_top_level(self, cfun, code):
        return ModuleLoader().recompile_top_level(self, cfun, code)

    def recompile_closure(self, cfun, code):
        return ModuleLoader().recompile_closure(self, cfun, code)

    # object routines
    # -dealing with nulls, booleans, integers, etc...

    def get_vt(self, obj):
        if obj == None:
            return core.Object
        elif isinstance(obj, basestring):
            return core.String
        elif isinstance(obj, dict) and '_vt' not in obj:
            return core.Dictionary
        elif isinstance(obj, int) or isinstance(obj, long):
            return core.Number
        elif isinstance(obj, list):
            return core.List
        elif isinstance(obj, Process):
            return core.VMProcess
        else:
            return obj["_vt"]

    def has_slot(self, obj, name):
        if obj == None:
            return False
        elif hasattr(obj, '__iter__'):
            return name in obj
        else:
            return False

    def alloc(self, klass, data):
        return _create_instance(klass, data)

    def interned_symbol_for(self, s):
        if s not in self.interned_symbols:
            self.interned_symbols[s] = self.alloc(core.Symbol, {'self':s})
        return self.interned_symbols[s]

    def shitty_get_module_from_cfunction(self, cfun):
        # I'm crying now...
        outer_cfun = cfun
        while outer_cfun['outer_cfun']:
            outer_cfun = outer_cfun['outer_cfun']
        return outer_cfun


class Process(greenlet):
    def __init__(self, interpreter):
        super(Process, self).__init__(self.greenlet_entry)

        self.interpreter = interpreter
        self.debugger_process = None
        self.state = None

    def greenlet_entry(self, cmd, *rest):
        return getattr(self, cmd)(*rest)

    def switch(self, *rest):
        # if self.interpreter.current_process:
        #     self.interpreter.current_process.state = 'paused'
        self.interpreter.current_process = self
        #print "switching..."
        return super(Process,self).switch(*rest)

    def run_module(self, entry_name, module, args):
        # registers
        # self.r_mp  = module
        # self.r_rp  = module # ie. module.main()
        # self.r_rdp = module

        # registers
        self.r_mp  = None  # module pointer
        self.r_cp  = None  # context pointer
        self.r_rp  = None  # receiver pointer
        self.r_rdp = None  # receiver data pointer
        self.r_ep  = None  # environment pointer
        self.r_ip  = None  # instruction pointer / ast info

        self.locals = {}
        self.stack = []

        self.state = 'running' # process state

        try:
            return self.setup_and_run_fun(module, module, entry_name, module[entry_name], args, True)
        except MemetalkException as e:
            print "exiting with memetalk exception: " + e.mmobj()['value']

    def pp_stack_trace(self):
        for frame in self.stack:
            if frame['r_cp']:
                print (frame['r_cp']['compiled_function']['name'] + " :::" + str(frame['r_cp']['compiled_function']['body']))[0:80] + "..."
        print (self.r_cp['compiled_function']['name'] + " ::: " + str(self.r_cp['compiled_function']['body']))[0:80] + "..."

    def _lookup(self, drecv, vt, selector):
        if vt == None:
            return None, None

        if selector in vt["dict"]:
            return drecv, vt["dict"][selector]
        else:
            parent = vt["parent"]
            if self.interpreter.has_slot(drecv, "_delegate"):
                delegate = drecv["_delegate"]
            else:
                delegate = None
            return self._lookup(delegate, parent, selector)

    # get the right rdp for ctors
    def ctor_rdp_for(self, rp, fun):
        if rp == None:
            raise Exception("No rdp for ctor. Probably a bug")

        klass = self.interpreter.get_vt(rp)['compiled_class']
        if klass == fun['compiled_function']['owner']:
            return rp
        else:
            return self.ctor_rdp_for(rp['_delegate'], fun)
        #if rp['_vt'] == fun

    def run_fun(self, recv, drecv, fun, args, should_allocate):
        self.setup_parameters_and_registers(recv, drecv, fun, args)
        if should_allocate and fun["compiled_function"]['is_ctor']:
            #allocate new instance and make it the receiver
            self.r_rp = self.interpreter.create_instance(self.r_rp)
            # rdp will be the instance associated to the class of fun
            self.r_rdp = self.ctor_rdp_for(self.r_rp, fun)
            #...updating env if exist
            if self.r_ep != None:
                self.r_ep["r_rp"] = self.r_rp
                self.r_ep["r_rdp"] = self.r_rdp

            #...fun will be executed with this new r_rp, below

        def evaluate(skip):
            if skip == True:
                self.state = 'running'
            self.evaluator = Eval([fun["compiled_function"]["body"]])
            self.evaluator.i = self
            try:
                #print 'evaluate '+fun['compiled_function']['name']+': ' + str(fun['compiled_function']['body'])
                ret,err = self.evaluator.apply("exec_fun")
                #print 'DONE evaluate '+fun['compiled_function']['name']+': ' + str(fun['compiled_function']['body'])
            except ReturnException as e:
                ret = e.val
            except RewindException as e:
                #print("Caugh rewind: " + str(e.count))
                if e.count > 1:
                    e.count = e.count - 1
                    #print("rewind more than one; tearing up and raising again")
                    self.tear_fun()
                    raise e
                #print("Rewind: NO tear up stack; just resume")
                return self.run_fun(recv, drecv, fun, args, should_allocate)
            except MemetalkException:
                self.tear_fun()
                raise
            except Exception as e:
                self.tear_fun()
                print "WARNING: python exception ocurred: " + str(e.__class__) + ":" + str(e)
                self.interpreter.throw_with_value(traceback.format_exc())
                raise
            self.tear_fun()
            if skip:
                self.state = 'paused'
            return ret
        ret = evaluate(self.state == 'next')
        #print 'evaluate '+fun['compiled_function']['name']+' DONE'
        # print "done fun:"
        # P(fun["compiled_function"]["body"],5)
        return ret

    def top_frame(self):
        return {"r_cp": self.r_cp,
                "r_rp": self.r_rp,
                "r_rdp": self.r_rdp,
                "r_ep" : self.r_ep,
                'r_ip' : self.r_ip,
                'locals':self.locals}

    def push_stack_frame(self):
        self.stack.append({
                "r_cp": self.r_cp,
                "r_rp": self.r_rp,
                "r_rdp": self.r_rdp,
                "r_ep" : self.r_ep,
                'r_ip' : self.r_ip,
                'locals':self.locals})
                # no need to backup mp, it can be infered from current fun
    def tear_fun(self):
        frame = self.stack.pop()
        self.r_cp = frame["r_cp"]
        self.r_rp = frame["r_rp"]
        self.r_rdp = frame["r_rdp"]
        self.r_ep  = frame["r_ep"]
        self.r_ip  = frame['r_ip']
        self.locals = frame['locals']
        if self.r_cp: #if we are exiting main, this will be null
            self.r_mp = self.r_cp["module"]

    def setup_and_run_fun(self, recv, drecv, name, method, args, should_allocate):
        if isinstance(method, basestring):
            traceback.print_exc()
            #print("function '" + name + "' lacks implementation. Receiver:");
            #P(recv)
            sys.exit(1)
        if len(method["compiled_function"]["params"]) != len(args):
            #P(method,3)
            self.interpreter.throw_with_value("arity error: " + method['compiled_function']['name'] +\
                                                  ", expecting " + str(len(method["compiled_function"]["params"])) +\
                                                  ", got " + str(len(args)) + " -- "+pformat(args,1,80,2))

        #backup frame
        self.push_stack_frame()
        return self.run_fun(recv, drecv, method, args, should_allocate)

    def setup_parameters_and_registers(self, recv, drecv, method, args):
        self.r_cp  = method
        self.r_mp = method["module"]
        self.r_ep = None
        self.locals = {}
        # binding up arguments to parameters
        if self.interpreter.get_vt(method) != core.Context:
            self.r_rp  = recv
            self.r_rdp = drecv
            if not method["compiled_function"]["uses_env"] and \
                    self.interpreter.get_vt(method) == core.Function:
                # normal fun, put args in the stack
                for k,v in zip(method["compiled_function"]["params"],args):
                    self.locals[k] = v
            # normal fun using env, initialize one
            elif method["compiled_function"]["uses_env"] and \
                    self.interpreter.get_vt(method) == core.Function:
                self.r_ep = dict(method["compiled_function"]['env_table_skel'])
                self.r_ep["r_rp"] = self.r_rp
                self.r_ep["r_rdp"] = self.r_rdp # usually receivers are on stack.
                                                # I need them here: when calling a
                                                # closure that references a 'this'
                # put args in the env
                for k,v in zip(method["compiled_function"]["params"],args):
                    self.env_set_value(k,v) # cp should be set already
        else:
            self.r_ep = method["env"]
            if 'r_rp' in self.r_ep:
                self.r_rp = self.r_ep['r_rp']
            if 'r_rdp' in self.r_ep:
                self.r_rdp = self.r_ep['r_rdp']
            # put args in the env
            for k,v in zip(method["compiled_function"]["params"],args):
                self.env_set_value(k, v)

    ## env auxiliary
    def env_lookup(self, name):
        #the idx of name in self.r_ep or None
        if self.r_ep == None:
            return None
        def lookup(cfun, name):
            if name in cfun["env_table"].values():
                return [k for k,v in cfun["env_table"].iteritems() if v == name][0]
            elif cfun["outer_cfun"]:
                return lookup(cfun["outer_cfun"], name)
            else:
                return None
        return lookup(self.r_cp["compiled_function"], name)

    def env_set_value(self, name, value):
        idx = self.env_lookup(name)
        if idx != None:
            self.r_ep[idx] = value
        else:
            self.interpreter.throw_with_value('Undeclared env variable: ' + name)

    def set_local_value(self, name, expr):
        if self.r_ep != None:
            self.env_set_value(name, expr)
        else:
            self.locals[name] = expr

    ##### eval routines

    def do_eval(self, name, *rest):
        try:
            return getattr(self, name)(*rest)
        except StandardError as e:
            print "WARNING: python exception ocurred: " + str(e.__class__) + ":" + str(e)
            self.interpreter.throw_with_value(traceback.format_exc())

    def eval_prim(self, prim_name, ast):
        self.r_ip = ast
        self.dbg_control('eval_prim')
        return globals()['prim_'+prim_name](self)

    def eval_do_field_assign(self, field, rhs, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_field_assign')

        if not field in self.r_rdp:
            self.interpreter.throw_with_value("object has no field " + field)
        else:
            self.r_rdp[field] = rhs

    def eval_do_local_assign(self, name,expr, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_local_assign')
        self.set_local_value(name, expr)

    def eval_do_var_def(self, name, expr, ast):
        self.r_ip = ast
        self.dbg_control('eval_var_def')
        self.set_local_value(name, expr)

    def eval_do_fun_lit(self, params, body, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_fun_lit')
        compiled_fun = self.r_cp["compiled_function"]["fun_literals"][id(body[0])]
        return _compiled_function_to_context(compiled_fun, self.r_ep, self.r_mp)

    def eval_access_field(self, field):
        if field in self.r_rdp:
            return self.r_rdp[field]
        else:
            self.interpreter.throw_with_value("object has no field " + field)

    def eval_do_access_index(self, left, idx, ast):
        return left[idx]

    def eval_access_module(self):
        return self.r_mp

    def eval_access_context(self):
        return self.r_cp

    def eval_access_this(self):
        return self.r_rp

    def eval_symbol(self, s):
        return self.interpreter.interned_symbol_for(s)

    def eval_access_var(self, name, ast):
        self.r_ip = ast
        self.dbg_control('eval_access_var')
        #-local
        if self.r_ep != None: # env
            idx = self.env_lookup(name)
            if idx != None:
                return self.r_ep[idx]
        else: # checking stack
            if name in self.locals:
                return self.locals[name]

        #-module params + module entries
        return self.lookup_in_modules(name, self.r_mp)

    def lookup_in_modules(self, name, mp):
        if name in mp:
            return mp[name]
        elif mp['_delegate'] != None:
            return self.lookup_in_modules(name, mp['_delegate'])
        else:
            self.interpreter.throw_with_value("Undeclared var: " + name + " : "+pformat(self.r_cp['compiled_function'],1,80,1))

    def eval_do_return(self, value, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_return')
        raise ReturnException(value)

    def eval_do_super_ctor_send(self, selector, args, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_super_ctor_send')
        # -super.foo() can only be used inside constructors.
        # -normal methods can invoke super() to get its superclass version.
        # -A method m() cannot invoke a super method of different name.
        if not self.r_cp["compiled_function"]["is_ctor"]:
            self.interpreter.throw_with_value("Cannot use super.m() outside ctor");

        instance = self.r_rdp
        klass = self.interpreter.get_vt(instance)
        pklass = klass["parent"]
        receiver = instance["_delegate"]

        drecv, method = self._lookup(receiver, self.interpreter.get_vt(pklass), selector)

        if not method:
            self.interpreter.throw_with_value("DoesNotUnderstand: " + selector + " -- " + pformat(instance,1,80,1))
        elif not method["compiled_function"]["is_ctor"]:
            self.interpreter.throw_with_value("Method is not constructor: " + selector)
        else:
            return self.setup_and_run_fun(self.r_rp, drecv, selector, method, args, False)

    def eval_do_bin_send(self, selector, receiver, arg, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_bin_send')
        drecv, method = self._lookup(receiver, self.interpreter.get_vt(receiver), selector)
        if not method:
            self.interpreter.throw_with_value("DoesNotUnderstand: " + selector + " -- " + pformat(receiver,1,80,1))
        else:
            return self.setup_and_run_fun(receiver, drecv, selector, method, [arg], True)

    def eval_do_un_not(self, value, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_un_not')
        return not value

    def eval_do_un_neg(self, value, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_un_neg')
        return - value

    def eval_do_send(self, receiver, selector, args, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_send')
        drecv, method = self._lookup(receiver, self.interpreter.get_vt(receiver), selector)
        if not method:
            self.interpreter.throw_with_value("DoesNotUnderstand: " + selector + " -- " + pformat(receiver,1,80,1))
        else:
            return self.setup_and_run_fun(receiver, drecv, selector, method, args, True)

    def eval_do_call(self, fun, args, ast):
        self.r_ip = ast
        self.dbg_control('eval_do_call')
        return self.setup_and_run_fun(self.r_rp, self.r_rdp, '<?>', fun, args, True)


    def eval_do_send_or_call(self, name, args,ast):
        self.r_ip = ast
        self.dbg_control('eval_do_send_or_call')
        fn = None
        # if name is local...
        if self.r_ep:
            idx = self.env_lookup(name) # in env?
            if idx != None:
                fn = self.r_ep[idx]
        elif name in self.locals:        # local in stack
            fn = self.locals[name]

        if fn:
            return self.setup_and_run_fun(self.r_rp, self.r_rdp, name, fn, args, True)

        #-module params + module entries
        def lookup_in_modules(name, mp):
            if name in mp:
                return mp[name]
            elif mp['_delegate'] != None:
                return lookup_in_modules(name, mp['_delegate'])
            else:
                self.interpreter.throw_with_value("Undeclared function: " + name)
        fn = lookup_in_modules(name, self.r_mp)
        return self.setup_and_run_fun(self.r_mp, self.r_mp, name, fn, args, True)

    def eval_do_if(self, cond, yes):
        if cond: #False/None vs. *
            self.evaluator.apply("exprlist", yes)

    def eval_do_if_else(self, cond, yes, no):
        if cond: #False/None vs. *
            self.evaluator.apply("exprlist", yes)
        else:
            self.evaluator.apply("exprlist", no)

    def eval_do_try(self, ast, tr, bind, ct):
        self.r_ip = ast
        try:
            ev = Eval([tr])
            ev.i = self
            return ev.apply("exprlist")[0]
        except MemetalkException as e:
            self.set_local_value(bind, e.mmobj())
            return self.evaluator.apply("exprlist", ct)[0]

    def eval_do_debug(self,ast):
        self.r_ip = ast
        self.state = 'paused'

    ## debugger

    def dbg_cmd(self, cmd):
        #print 'dbg_cmd received: ' + str(cmd)
        if cmd == 'step_into':
            #print("process:: changed state to paused")
            self.state = 'paused'
        if cmd == 'step_over':
            #print("process:: changed state to next")
            self.state = 'next'
        if cmd == 'continue':
            print("process:: changed state to continue")
            self.state = 'running'
        if cmd == 'rewind':
            #print("process: rewinding 1")
            raise RewindException(1)

    def dbg_control(self, name):
        if self.state == 'paused' or self.state == 'next':
            if not self.debugger_process:
                #print 'dbg_control: paused: initiating debugger...please wait'
                cmd = self.interpreter.debug_process(self)
            else:
                #print 'dbg_control paused: asking debugger for cmd...'
                cmd = self.debugger_process.switch()
            #print 'dbg_control got: ' + str(cmd)
            self.dbg_cmd(cmd)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        print "i.py filename"
        sys.exit(0)
    Interpreter().start(sys.argv[1])
