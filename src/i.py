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

import parser
import sys
import os
from parser import MemeParser
from loader import Loader
from evaluator import Eval
from prim import *
from prim import _create_closure, _compiled_function_as_context_with_frame, _context_with_frame
import core_module as core
from pdb import set_trace as br
import traceback
from config import MODULES_PATH, CURRENT_PATH
from astbuilder import *
from jinja2 import Environment
from mmpprint import P
import multiprocessing
from mobject import Dict, id_eq
import ipc
import atexit
import sys
import time

logger = logging.getLogger("i")

#######################################################
## Loading
#######################################################


class ModuleLoader(ASTBuilder):
    def __init__(self):
        self.first_fnlit = None
        self.recompiling_cfun = None

    def recompile_top_level(self, proc, cfun, src):
        self.line_offset = 0
        self.pos_stack = []
        name = cfun['name']

        self.interpreter = proc.interpreter

        self.recompiling_cfun = cfun

        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("single_top_level_fun", name)
        except Exception as err:
            if hasattr(err,'formatError'):
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())

        logger.debug("---- AST ----")
        logger.debug(ast)
        logger.debug("//---- AST ----")

        self.env_id_table = []
        self.env_idx = 0
        self.fun_literals = []

        self.functions = [self.recompiling_cfun]

        loader = Loader([ast])
        loader.i = self

        try:
            owner = cfun['owner']
            if id_eq(owner['_vt'], self.interpreter.core_imod['CompiledClass']):
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
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())


    def recompile_closure(self, proc, cfun, src):
        self.line_offset = 0
        self.pos_stack = []

        self.interpreter = proc.interpreter

        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("funliteral")
        except Exception as err:
            if hasattr(err,'formatError'):
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())

        logger.debug("---- AST ----")
        logger.debug(ast)
        logger.debug("//---- AST ----")

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
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())
        return cfun

    def compile_closure(self, proc, src, outer):
        self.line_offset = 0
        self.pos_stack = []

        self.interpreter = proc.interpreter

        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("funliteral")
        except Exception as err:
            if hasattr(err,'formatError'):
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())

        logger.debug("---- AST ----")
        logger.debug(ast)
        logger.debug("//---- AST ----")

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
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())

        return self.first_fnlit

    def compile_top_level(self, proc, name, src, owner, flag):
        self.line_offset = 0
        self.pos_stack = []

        self.interpreter = proc.interpreter

        if id_eq(owner['_vt'], self.interpreter.core_imod['CompiledClass']):
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
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())

        logger.debug("---- AST ----")
        logger.debug(ast)
        logger.debug("//---- AST ----")

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
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())

        if flag['self'] == "class_method" or flag['self'] == "constructor":
            return self.current_class["own_methods"][name]
        elif flag['self'] == 'instance_method':
            return self.current_class["methods"][name]
        else:
            return self.current_module["compiled_functions"][name]

    def compile_module(self, proc, name, src):
        self.line_offset = 0
        self.pos_stack = []

        self.interpreter = proc.interpreter

        logger.debug("Compiling module: " + name + ". this may take a while...")
        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("start")
        except Exception as err:
            if hasattr(err,'formatError'):
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())

        logger.debug("---- AST ----")
        logger.debug(ast)
        logger.debug("//---- AST ----")

        self.current_module = proc.interpreter.create_compiled_module({"name": name,
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
                proc.throw_with_message(err.formatError(''.join(self.parser.input.data)))
            else:
                proc.throw_py_exception(err, traceback.format_exc())

        logger.debug("module compiled: " + name)

        return self.current_module

    def l_module_license(self, lic):
        self.current_module["license"] = lic

    def l_module_params(self, params):
        self.current_module["params"] = params

    def l_default_p_lib(self, name, spec, args):
        self.current_module['default_params'][name] = {'name': name, 'type':'lib','value':spec[1]}

    def l_default_p_uri(self, name, uri, args):
        self.current_module['default_params'][name] = {'name': name, 'type':'uri','value':spec[1]}

    def l_module_alias(self, libname, aliases):
        self.current_module['aliases'].append((libname, aliases))

    def l_begin_class(self, name, super_class, fields):
        self.current_class = self.interpreter.create_compiled_class({"name":name,
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
            self.functions.append(self.interpreter.create_compiled_function({"name":name,
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
        cfun = self.interpreter.create_compiled_function({"name":"<anonymous>",
                                                          "outer_cfun": self.functions[-1],
                                                          "owner": self.functions[-1]['owner'],
                                                          "is_top_level": False,
                                                          "@tag":"a compiled literal function"})
        self.functions.append(cfun)

    def l_enter_first_literal_fun(self):
        self.env_id_table.append({})
        if self.first_fnlit == None:
            self.first_fnlit = self.interpreter.create_compiled_function({"name":"<anonymous>",
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

        body.cfun_literal = function
        # self.functions[-1]["fun_literals"][body._id] = function

        # print "REGISTERED CLOSURE:"
        # print body._id
        # P(body)
        # print '---- ON ----'
        # P(self.functions[-1],2)

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
    def __init__(self, mmobj, mtrace, pyex = None, pytrace = None):
        self._mmobj = mmobj
        self._mmobj['mtrace'] = mtrace
        self._mmobj['py_exception'] = pyex
        if pytrace == None:
            self._mmobj['py_trace'] = "".join(traceback.format_stack()[:-1])
        else:
            self._mmobj['py_trace'] = pytrace
    def mmobj(self):
        return self._mmobj

class Interpreter():
    def __init__(self):
        self.manager = multiprocessing.Manager()

        self.shared = self.manager.Namespace()
        self.shared.instance_ids = 0

        # DON'T use manager.dict and stuff here!!
        # each *access* to an entry of manager shit
        # creates a different copy of the object.
        #   ie. c = self.compiled_modules[module_name]
        #       creates another copy of the cmodule into c!
        # which means we end up with two versions of mm objects
        # which means we can't track them to patch them

        self.compiled_modules = {}
        self.imods = []
        self.interned_symbols = {}
        self.instances_by_class = {} #[klass_id] = [i1, i2, ..., in]
        self.objects = {}

        self.processes = {}
        self.procids = 0

        self.core_imod = {}
        core.init(self, self.core_imod)


        self.my_channel = self.manager.Queue()

        atexit.register(self.cleanup)

    ### Object creation ###

    def create_compiled_function(self, data):
        template = {"_vt": self.core_imod['CompiledFunction'],
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
        return self.new_object(dict(template.items() + data.items()))

    def create_compiled_class(self, data):
        template = {"_vt": self.core_imod['CompiledClass'],
                    "_delegate": None,
                    "module": None, # compiled module
                    "name": "",
                    "super_class_name":"Object",
                    "fields": [],
                    "methods": {},
                    "own_methods":{},
                    "@tag":"a CompiledClass"}
        return self.new_object(dict(template.items() + data.items()))

    def create_module(self, data):
        template = {"_vt": self.core_imod['ModuleBehavior'],
                    "_delegate": None,
                    "parent": self.core_imod['Object'],
                    "dict": {},
                    "compiled_module": None,
                    "@tag": "a Module"}
        return self.new_object(dict(template.items() + data.items()))

    def create_class(self, data):
        template = {"_vt": None, # it should be given by data [FooClassBehavior]
                    "_delegate": None,
                    "parent": None,
                    "dict": {},
                    "compiled_class":None,
                    "@tag":"a class"}
        return self.new_object(dict(template.items() + data.items()))


    def create_accessor_method(self, imodule, name):
        cf =  self.create_compiled_function({
                "name": name,
                "body":  [ASTNode(['return', ['field', name]],'return @'+ name +';',0,0,0,0)]})
        return self.function_from_cfunction(cf, imodule)

    def function_from_cfunction(self, cfun, imodule):
        return self.new_object({"_vt": self.core_imod['Function'],
                                "_delegate": None,
                                "compiled_function": cfun,
                                "module": imodule,
                                "@tag": "a Function"})

    def compiled_function_to_context(self, cfun, env, imodule):
        return self.new_object({"_vt": self.core_imod['Context'],
                                "_delegate": None,
                                "compiled_function": cfun,
                                "env":env,
                                "module": imodule,
                                "@tag": "a Context"})

    def compiled_functions_to_functions(self, cfuns, imodule):
        funs = {}
        for name, fun in cfuns.items():
            funs[name] = self.function_from_cfunction(fun, imodule)
        return funs

    def create_compiled_module(self, data):
        template = {"_vt": self.core_imod['CompiledModule'],
                    "_delegate": None,
                    "name": "",
                    "license":"",
                    "params": [],
                    "default_params": {},
                    "aliases": [],
                    "compiled_functions": {},
                    "compiled_classes": {},
                    "@tag":" a CompiledModule"}

        return self.new_object(dict(template.items() + data.items()))


    def instantiate_module(self, proc, compiled_module, _args, parent_module):
        logger.debug("Instantiating module: " + compiled_module['name'])
        def setup_module_arguments(_args, compiled_module):
            args = dict(_args)
            # module's default argument
            for name,dp in compiled_module['default_params'].iteritems():
                if name not in args.keys():
                    if dp['type'] == 'lib':
                        args[name] = self.compile_module_lib(proc, dp['value'])
                    elif dp['type'] == 'uri':
                        args[name] = self.compile_module_uri(proc, dp['value'])
                    else:
                        proc.throw_with_message('Unknown module spec')
            if sorted(args.keys()) != sorted(compiled_module['params']):
                proc.throw_with_message('arity error on module parameters')

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
        imod_dictionary = {"_compiledModule":self.function_from_cfunction(
                self.create_compiled_function({
                        'name':'_compiledModule',
                        'params':[],
                        'body': ['primitive', ['literal-string', 'module_instance_compiled_module']],
                        '@tag': '<Module>._compiledModule compiled function'}),
                self.core_imod)}

        # Module
        module = self.create_module({"_vt": self.core_imod['ModuleBehavior'],
                                     "dict": imod_dictionary,
                                     "compiled_module": compiled_module,
                                     "@tag":"Module " + compiled_module["name"]})

        # module instance
        imodule = self.new_object({"_vt": module,
                                   "_delegate": parent_module,
                                   "@tag":"Module instance: " + compiled_module["name"]})

        # accessors
        for cname, cclass in compiled_module["compiled_classes"].items():
            imod_dictionary[cname] = self.create_accessor_method(imodule, cname)

        # functions as module methods
        for cfname, cfun in compiled_module["compiled_functions"].items():
            imod_dictionary[cfname] = self.function_from_cfunction(cfun,imodule)
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
                proc.throw_with_message("super class not found: " + super_name)

            # superclass BarClass found
            if super_class:
                # FooClassBehavior
                cbehavior = proc.interpreter.new_object({"_vt": self.core_imod['Behavior'],
                                                    "parent": super_class["_vt"],
                                                    "dict": self.compiled_functions_to_functions(c["own_methods"], imodule),
                                                    "@tag":c["name"]+" Behavior"})

                classes[c["name"]] = self.create_class({"_vt": cbehavior,
                                                        "parent": super_class,
                                                        "dict": self.compiled_functions_to_functions(c["methods"],imodule),
                                                        "compiled_class":c,
                                                        "@tag":c["name"]+" Class"})
                bclasses[c["name"]] = cbehavior
            else:
                # FooClassBehavior
                cbehavior = proc.interpreter.new_object({"_vt": self.core_imod['Behavior'],
                                                         "parent": "*replace-me*", #latter below
                                                         "dict": self.compiled_functions_to_functions(c["own_methods"],imodule),
                                                         "@tag":c["name"]+" Behavior"})

                bclasses[c["name"]] = cbehavior
                # superclass BarClass will be eventually in the variable 'classes'
                classes[c["name"]] = self.create_class({"_vt": cbehavior,
                                                        "parent": "*replace-me*", #placeholder to substitute later below
                                                        "dict": self.compiled_functions_to_functions(c["methods"],imodule),
                                                        "compiled_class":c,
                                                        "@tag":c["name"]+" Class"})

        for name, super_name in super_later.items():
            classes[name]["parent"] = classes[super_name]
            classes[name]["_vt"]["parent"] = bclasses[super_name]

        # #update parent classes
        # for name, c in classes.items():
        #     parent_name = classes[name]["parent"]

        funs = self.compiled_functions_to_functions(compiled_module["compiled_functions"], imodule)

        for name, fun in funs.items():
            imodule[name] = fun
        for name, klass in classes.items():
            imodule[name] = klass

        for name, val in args.iteritems():
            imodule[name] = val

        self.imods.append(imodule)
        return imodule

    #####################

    def cleanup(self):
        try:
            for k,p in self.processes.iteritems(): # list of your processes
                p.terminate() # supported from python 2.6
        except:
            pass

    def module_to_text(self, cmod):
        def if_ctor(lst, val):
            return [(name, obj) for name, obj in lst if obj["is_ctor"] == val]

        def comment(text, module_name, cname, name):
            if text.count("\n") > 40:
                if cname:
                    return text + "\n" + "//end " + module_name + ":" + cname + ":" + name
                else:
                    return text + "\n" + "//end " + module_name + ":" + name
            else:
                return text

        src = open(CURRENT_PATH + "/templates/module.tpl.mm").read()
        env = Environment(trim_blocks=True, keep_trailing_newline=True)
        env.filters['if_ctor'] = if_ctor
        env.filters['comment'] = comment
        template = env.from_string(src)
        defaults = [{"name":name,"value":spec["value"]} for name, spec in cmod["default_params"].iteritems()]
        aliases = [{"list": ', '.join(alias[1]),"from":alias[0]} for alias in cmod["aliases"]]

        args = {"module_parameters": ', '.join(cmod['params']),
                "default_parameters": defaults,
                "aliases": aliases,
                "module": cmod}
        return template.render(args)

    def save_module(self, proc, name):
        ## Empty fields are being dumped as "fields ;"!!
        logger.info("saving module: " + name)
        cmod = self.compiled_module_by_filename(proc, name + ".mm")
        source = self.module_to_text(cmod)
        logger.info("saving to: " + os.path.join(MODULES_PATH, name + ".mm"))
        open(os.path.join(MODULES_PATH, name + ".mm"), "w").write(source)

    def open_module_file(self, filename):
        if os.path.isfile(filename):
            return open(filename)
        else:
            return open(os.path.join(MODULES_PATH, filename))

    def spawn(self, target_procid = None):
        if target_procid != None:
            logger.debug('spawn: creating debugger for: ' + str(target_procid))
            logger.debug('target process channel is: ' + str(self.processes[target_procid].channels['my']))
            process = Process(self, self.procids, self.processes[target_procid].channels['my'])
        else:
            logger.debug('spawn: creating regular proc')
            process = Process(self, self.procids)
        self.processes[self.procids] = process
        self.procids += 1
        return process

    def start(self, filename):
        proc = self.spawn()
        logger.debug('start:exec_module: ' + str(proc.procid))
        proc.setup('exec_module', filename, 'main', [])
        #proc.run()
        proc.start()
        self.interpreter_loop()

    def maybe_exit(self):
        if len(self.processes) == 0:
            sys.exit(0)

    def interpreter_loop(self):
        # Note: we can't receive nor send recursive/structured data here,as
        # this would inject alien meme objects and comparisions would overflow
        # the stack.

        while True:
            logger.debug("interpreter_loop: WAIT....")
            msg = ipc.get(self.my_channel)
            logger.debug("interpreter_loop: RECV: " + msg['name'])
            getattr(self, 'cmdproc_' + msg['name'])(*msg['args'])

    def cmdproc_spawn(self):
        logger.debug("interpreter_loop: SEND created procid")
        ipc.put(self.my_channel, self.spawn().procid)

    def cmdproc_proc_exec_fun(self, procid, fn, args, state = 'running'):
        self.processes[procid].setup('exec_fun', fn, args, state)
        self.processes[procid].start()
        logger.debug("interpreter_loop: SEND done exec_fun")
        ipc.put(self.my_channel, 'done')

    def cmdproc_kill_process(self, procid):
        self.processes[procid].terminate()
        del self.processes[procid]
        self.maybe_exit()

    def cmdproc_process_terminated(self, procid):
        del self.processes[procid]
        self.maybe_exit()

    # def cmdproc_exec_module(self, procid, mname, fname, args):
    #     proc = self.processes[procid]
    #     proc.exec_module(mname + ".mm", fname, args)

    def cmdproc_debug(self, procid, new_state, last_exception):
        # 1) pause procid
        # 2) create debugger with procid as target

        logger.debug('cmdproc_debug: spawning debugger for target: ' + str(procid))
        dbgproc = self.spawn(procid)
        logger.debug('cmdproc_debug: debugger procid: ' + str(dbgproc.procid))

        proc = self.processes[procid]
        proc.channels['dbg'] = dbgproc.channels['target_incoming']
        proc.shared['mydbg.pid'] = dbgproc.procid
        logger.debug('cmdproc_debug: SEND new state for target:' + new_state)
        ipc.put(proc.channels['my'], {'cmd': 'dbg_break', 'value': new_state})

        logger.debug('cmdproc_debug: telling debugger to start() running idez::debug()')

        dbgproc.setup('exec_module', 'idez.mm', 'debug', [procid, last_exception])
        dbgproc.start()
        logger.debug('interpreter: cmdproc_debug SEND dbg_done to who called us')
        ipc.put(self.my_channel, 'dbg_done')

    def cmdproc_update_object(self, procid_origin, obj):
        updated = False
        for _, proc in self.processes.iteritems():
            if proc.procid != procid_origin:
                updated = True
                logger.debug("SEND to proc " + str(proc.procid) + ": update object " + str(obj["@id"]))
                ipc.put(proc.channels['my'], {'cmd': 'update_object', "value": obj})
        if not updated:
            logger.debug("Interpreter::cmd_update_object: no need to update " + str(obj["@id"]))

    def cmdproc_update_cmodule(self, procid_origin, cmodule):
        logger.debug("Interpreter: updating our cmodule...")
        self.compiled_modules[cmodule['name']] = cmodule
        updated = False
        for _, proc in self.processes.iteritems():
            if proc.procid != procid_origin:
                updated = True
                logger.debug("SEND to proc " + str(proc.procid) + ": update cmodule " + str(cmodule["@id"]))
                ipc.put(proc.channels['my'], {'cmd': 'update_cmodule', "value": cmodule})
        if not updated:
            logger.debug("Interpreter::cmd_update_cmodule: no need to update " + str(cmodule["@id"]))

    def compiled_module_by_filename(self, proc, filename):
        module_name = filename[:-3]
        if module_name not in self.compiled_modules:
            source = self.open_module_file(filename).read()
            self.compiled_modules[module_name] =  ModuleLoader().compile_module(proc,module_name,source)
            proc.call_interpreter(False, 'update_cmodule', proc.procid, self.compiled_modules[module_name])
        return self.compiled_modules[module_name]

    def compiled_module_by_filepath(self, proc, filepath):
        module_name = filepath
        if module_name not in self.compiled_modules:
            source = open(filepath).read()
            self.compiled_modules[module_name] =  ModuleLoader().compile_module(proc,module_name,source)
            proc.call_interpreter(False, 'update_cmodule', proc.procid, self.compiled_modules[module_name])
        return self.compiled_modules[module_name]

    def imodules(self):
        return self.imods

    def compile_module_lib(self, proc, name):
        compiled_module = self.compiled_module_by_filename(proc, name + '.mm')
        imod = self.instantiate_module(proc, compiled_module, {}, self.core_imod)
        return imod

    def compile_module_uri(self, proc, uri):
        proc.throw_with_message('TODO: compile_module_uri')

    def kernel_module_instance(self):
        return self.core_imod.kernel_imodule

    def get_core_class(self, name):
        return self.core_imod[name]

    def get_core_module(self):
        return self.core_imod

    def py_memetalk_exception(self):
        return MemetalkException

    def compile_top_level(self, proc, name,text, owner, flags):
        return ModuleLoader().compile_top_level(proc, name, text, owner, flags)

    def compile_closure(self, proc, text, cfun):
        return ModuleLoader().compile_closure(proc, text, cfun)

    def recompile_top_level(self, proc, cfun, code):
        return ModuleLoader().recompile_top_level(proc, cfun, code)

    def recompile_closure(self, proc, cfun, code):
        return ModuleLoader().recompile_closure(proc, cfun, code)

    # object routines
    # -dealing with nulls, booleans, integers, etc...

    def get_vt(self, obj):
        if obj == None:
            return self.core_imod['Object']
        elif isinstance(obj, basestring):
            return self.core_imod['String']
        elif isinstance(obj, dict) and '_vt' not in obj:
            return self.core_imod['Dictionary']
        elif isinstance(obj, int) or isinstance(obj, long):
            return self.core_imod['Number']
        elif isinstance(obj, list):
            return self.core_imod['List']
        elif isinstance(obj, Process):
            return self.core_imod['VMProcess']
        else:
            return obj["_vt"]

    def has_slot(self, obj, name):
        if obj == None:
            return False
        elif hasattr(obj, '__iter__'):
            return name in obj
        else:
            return False

    def is_wrapper_class(self, klass):
        return id_eq(klass, self.core_imod['String']) or\
            id_eq(klass, self.core_imod['Dictionary']) or\
            id_eq(klass, self.core_imod['List']) or\
            id_eq(klass, self.core_imod['Number'])

    def new_wrapper_object_for(self, klass):
        if id_eq(klass, self.core_imod['String']):
            return str()
        if id_eq(klass, self.core_imod['Dictionary']):
            return dict()
        if id_eq(klass, self.core_imod['List']):
            return list()
        if klass.Number:
            return 0

    def create_instance(self, klass):
        p = None
        if klass["parent"] != None:
            p = self.create_instance(klass["parent"])

        if self.is_wrapper_class(klass):
            return self.new_wrapper_object_for(klass)

        fields = klass["compiled_class"]["fields"]
        name = klass["compiled_class"]["name"]
        res = dict([(x,None) for x in fields] + {"_vt": klass, "_delegate":p, "@tag": name + " instance"}.items())

        obj = self.new_object(res)
        return obj


    def new_object(self, data = {}):
        assert(type(data) == dict)

        obj = Dict(data)

        if '@id' in obj:
            raise Exception("BUG: @id already set")

        obj['@id'] = self.shared.instance_ids
        self.shared.instance_ids += 1

        self.objects[obj['@id']] = obj

        # tracking instances associated to compiled classes
        # so we can patch them when meta modifications are made
        # to the class
        if '_vt' in obj and 'compiled_class' in obj['_vt']:
            key = obj['_vt']['compiled_class']['@id']
            if key not in self.instances_by_class:
                self.instances_by_class[key] = {}
            self.instances_by_class[key][obj['@id']] = obj

        return obj

    def alloc_object(self, klass, data):
        template = {"_vt": klass,
                "_delegate": None}
        return self.new_object(dict(template.items() + data.items()))

    def interned_symbol_for(self, s):
        if s not in self.interned_symbols:
            self.interned_symbols[s] = self.alloc_object(self.core_imod['Symbol'], {'self':s})
        return self.interned_symbols[s]

    def shitty_get_module_from_cfunction(self, cfun):
        # I'm crying now...
        outer_cfun = cfun
        while outer_cfun['outer_cfun']:
            outer_cfun = outer_cfun['outer_cfun']
        return outer_cfun

    def object_become(self, obj):
        oid = obj['@id']
        logger.debug("Become'ing an object: " + str(oid) + " :: " + str(id(obj)))
        if oid in self.objects:
            logger.debug("Become: object " + str(oid) + " found")
            local = self.objects[oid]
            local.clear()
            local.update(obj)
        else:
            logger.debug("Become: obj not found, adding " + str(obj["@id"]))
            self.objects[obj['@id']] = obj

class Process(multiprocessing.Process):
    def __init__(self, interpreter, procid, target_channel = None):
        super(Process, self).__init__()

        self.procid = procid

        self.interpreter = interpreter
        self.mydbg = None

        self.mm_self_obj = None

        self.shared = multiprocessing.Manager().dict()

        self.channels = multiprocessing.Manager().dict()

        # channel where I talk to my interpreter
        self.channels['interpreter'] = interpreter.my_channel

        # channel where I receive commands from outside (ie. a debugger)
        self.channels['my'] = interpreter.manager.Queue()

        # channel where I command a target process (ie. I'm a debugger)
        self.channels['target'] = target_channel

        # channel where I send info to my debugger  (ie. I'm a target process)
        self.channels['dbg'] = None

        # channel where I receive info from my target (ie. I'm a debugger)
        self.channels['target_incoming'] = interpreter.manager.Queue()

        self.volatile_breakpoints = []

        self.entry = {} # data for executing the entry point

        self.state = None

        self.flag_debug_on_exception = False
        self.exception_protection = [False]
        self.last_exception = None

        self.init_data()

    def init_data(self):
        # top frame / registers
        self.tframe = self.interpreter.new_object({'r_mp':None,    # module pointer
                                                   'r_cp':None,    # context pointer
                                                   'r_rp': None,   # receiver pointer
                                                   'r_rdp': None,  # receiver data pointer
                                                   'r_ep': None,   # environment pointer
                                                   'r_ip':None,   # instruction pointer / ast info
                                                   'locals':{}})
        self.stack = []


    def mm_self(self):
        if not self.mm_self_obj:
            VMProcess = self.interpreter.get_core_class('VMProcess')
            # VMStackFrameClass = self.interpreter.get_core_class('VMStackFrame')
            # frames = [self.interpreter.alloc_object(VMStackFrameClass,{'self':x}) for x in self.all_stack_frames()]
            self.mm_self_obj = self.interpreter.alloc_object(VMProcess, {'procid': self.procid})
        return self.mm_self_obj

    def break_at(self, cfun, line):
        logger.debug("Breaking at: " + str(line) + " --- " + P(cfun,1,True))
        self.volatile_breakpoints.append({"cfun":cfun, "line":line})

    def init_debugging_session(self):
        logger.debug(str(self.procid) + ': init_debugging_session(). WAIT...')
        data = ipc.get(self.channels['target_incoming'])
        logger.debug(str(self.procid) + ': RECV: should receive "done dbg_break":' + P(data,1,True))
        return self.receive_frames()

    def receive_frames(self):
        logger.debug(str(self.procid) + ': receive_frames(). WAIT...')
        data = ipc.get(self.channels['target_incoming'])
        logger.debug(str(self.procid) + ': RECV: received frames:' + P(data,1,True))
        return data

    def call_target_process(self, block, procid, name, *args):
        logger.debug(str(self.procid) + ": Process::call_target_process()")
        logger.debug(str(self.procid) + ': queue empty?' + str(self.channels['target'].empty()))
        logger.debug(str(self.procid) + ': SEND putting data on queue....')
        ipc.put(self.channels['target'], {"cmd":name, "args": args})
        logger.debug(str(self.procid) + ': data sent. Should we block?' + str(block))
        if block:
            logger.debug(str(self.procid) + ': call_target_process WAIT for debugged process to answer')
            data = ipc.get(self.channels['target_incoming'])#self.channels['my'].get()
            logger.debug(str(self.procid) + ': RECV debugger got data from target. returning it')
            return data
        logger.debug(str(self.procid) + "call_target_process() DONE")

    def call_interpreter(self, block, name, *args):
        logger.debug('call_interpreter SEND: ' + name)
        ipc.put(self.channels['interpreter'], {"name":name, "args":args})
        if block:
            logger.debug('call_interpreter WAITing for result')
            res = ipc.get(self.channels['interpreter'])
            logger.debug('call_interpreter RECV result: ' + str(res))
            return res
        else:
            return None

    def debug_me(self, new_state = 'paused'):
        if self.channels['dbg'] == None:
            self.call_interpreter(True, 'debug', self.procid, new_state, self.last_exception)
        else:
            logger.debug(str(self.procid) + ": debug_me(): we have a dbg; pausing...")
            self.state = new_state

    def setup(self, name, *args):
        self.entry = {'name': name, 'args': args}

    def run(self):
        logger.info("running process:" + str(self.procid))
        self.init_data()
        getattr(self, self.entry['name'])(*self.entry['args'])

    def exec_fun(self, fn, args, state = 'running'):
        try:
            logger.debug(str(self.procid) + ': START exec_fun, state: ' + state)

            self.state = 'running'

            if state == 'paused':
                self.debug_me()

            ret = self.setup_and_run_fun(None,
                                         None,
                                         fn['compiled_function']['name'],
                                         fn,
                                         args, True)

            print str(self.procid) + ": RETVAL: " + P(ret,1,True)
        except MemetalkException as e:
            print (str(self.procid) + ": Exception raised during exec_module : " + e.mmobj()['message'])
            print (e.mmobj()['py_trace'])

        if self.channels['dbg'] != None: # we have a debugger, kill it
            self.call_interpreter(False, 'kill_process', self.shared['mydbg.pid'])

        self.call_interpreter(False, 'process_terminated', self.procid)

    def exec_module(self, filename, entry, args, state = 'running'):
        try:
            logger.debug(str(self.procid) + ': START exec_module: ' + filename)
            self.state = state
            logger.debug(str(self.procid) + ': exec_module: getting compiled module...')
            compiled_module = self.interpreter.compiled_module_by_filename(self,filename)
            imodule = self.interpreter.instantiate_module(self, compiled_module, {}, self.interpreter.core_imod)

            ret = self.setup_and_run_fun(imodule,
                                         imodule,
                                         entry,
                                         imodule[entry],
                                         args, True)

            print str(self.procid) + ": RETVAL: " + P(ret,1,True)
        except MemetalkException as e:
            print (str(self.procid) + ": Exception raised during exec_module : " + e.mmobj()['message'])
            print (e.mmobj()['py_trace'])

        if self.channels['dbg'] != None: # we have a debugger, kill it
            self.call_interpreter(False, 'kill_process', self.shared['mydbg.pid'])

        self.call_interpreter(False, 'process_terminated', self.procid)

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
        if id_eq(klass, fun['compiled_function']['owner']):
            return rp
        else:
            return self.ctor_rdp_for(rp['_delegate'], fun)
        #if rp['_vt'] == fun

    def run_fun(self, recv, drecv, fun, args, should_allocate):
        self.setup_parameters_and_registers(recv, drecv, fun, args)
        if should_allocate and fun["compiled_function"]['is_ctor']:
            #allocate new instance and make it the receiver
            self.reg('r_rp', self.interpreter.create_instance(self.reg('r_rp')))
            # rdp will be the instance associated to the class of fun
            self.reg('r_rdp', self.ctor_rdp_for(self.reg('r_rp'), fun))
            #...updating env if exist
            if self.reg('r_ep') != None:
                self.reg('r_ep')["r_rp"] = self.reg('r_rp')
                self.reg('r_ep')["r_rdp"] = self.reg('r_rdp')

            #...fun will be executed with this new r_rp, below

        def evaluate(skip):
            if skip == True:
                self.state = 'running'
            self.evaluator = Eval([fun["compiled_function"]["body"]])
            self.evaluator.i = self
            try:
                ret,err = self.evaluator.apply("exec_fun")
            except ReturnException as e:
                ret = e.val
            except RewindException as e:
                if e.count > 1:
                    e.count = e.count - 1
                    self.tear_fun()
                    raise e
                return self.run_fun(recv, drecv, fun, args, should_allocate)
            except MemetalkException:
                self.tear_fun()
                raise
            except Exception as e:
                self.tear_fun()
                raise
            self.tear_fun()
            if skip:
                self.state = 'paused'
            return ret
        ret = evaluate(self.state == 'next')
        return ret

    def reg(self, name, *args):
        if len(args) == 0:
            return self.tframe[name]
        else:
            self.tframe[name] = args[0]

    def locals(self, *args):
        if len(args) == 0:
            return self.tframe['locals']
        else:
            self.tframe['locals'] = args[0]

    def frame_level(self, frame):
        if id_eq(frame, self.tframe):
            return 0
        else:
            return list(reversed(self.all_stack_frames())).index(frame)

    def all_stack_frames(self):
        return self.stack[1:] + [self.top_frame()]

    def frame_by_index(self, idx):
        if len(self.all_stack_frames()) == idx:
            return self.top_frame()
        else:
            return self.all_stack_frames()[idx]

    def top_frame(self):
        return self.tframe

    def push_stack_frame(self):
        self.stack.append(self.tframe)
        self.tframe = self.interpreter.new_object()

    def tear_fun(self):
        self.tframe = self.stack.pop()

    def has_vararg(self, params):
        return len(params) > 0 and len(params[-1]) == 2 and params[-1][0] == 'var-arg'

    def ok_arity(self, params, arglen):
        if self.has_vararg(params):
            return arglen >= (len(params)-1)
        else:
            return arglen == len(params)

    def setup_and_run_fun(self, recv, drecv, name, method, args, should_allocate):
        if isinstance(method, basestring):
            logger.debug("*ERROR: function '" + name + "' lacks implementation. Receiver:");
            sys.exit(1)
        if not self.ok_arity(method["compiled_function"]["params"], len(args)):
            #P(method,3)
            self.throw_with_message("arity error: " + method['compiled_function']['name'] +\
                                                  ", expecting " + str(len(method["compiled_function"]["params"])) +\
                                                  ", got " + str(len(args)) + " -- "+ P(args,1,True) + ", receiver: " + P(recv,2,True))

        #backup frame
        self.push_stack_frame()
        return self.run_fun(recv, drecv, method, args, should_allocate)

    def setup_parameters_and_registers(self, recv, drecv, method, args):
        self.reg('r_cp', method)
        self.reg('r_mp', method["module"])
        self.reg('r_ep', None)
        self.locals({})
        if not id_eq(self.interpreter.get_vt(method), self.interpreter.core_imod['Context']):
            self.reg('r_rp', recv)
            self.reg('r_rdp', drecv)

            if not method["compiled_function"]["uses_env"] and \
                    id_eq(self.interpreter.get_vt(method), self.interpreter.core_imod['Function']):
                # normal fun, put args in the stack
                if self.has_vararg(method["compiled_function"]["params"]):
                    regular = method["compiled_function"]["params"][0:-1]
                    reg_len = len(regular)
                    for k,v in zip(regular,args):
                        self.locals()[k] = v
                    va_name = method["compiled_function"]["params"][-1][1]
                    self.locals()[va_name] = args[reg_len:]
                else:
                    for k,v in zip(method["compiled_function"]["params"],args):
                        self.locals()[k] = v
            # normal fun using env, initialize one
            elif method["compiled_function"]["uses_env"] and \
                    id_eq(self.interpreter.get_vt(method), self.interpreter.core_imod['Function']):
                self.reg('r_ep', dict(method["compiled_function"]['env_table_skel']))
                self.reg('r_ep')["r_rp"] = self.reg('r_rp')
                self.reg('r_ep')["r_rdp"] = self.reg('r_rdp') # usually receivers are on stack.
                                                # I need them here: when calling a
                                                # closure that references a 'this'
                # put args in the env
                for k,v in zip(method["compiled_function"]["params"],args):
                    self.env_set_value(k,v) # cp should be set already
        else:
            self.reg('r_ep', method["env"])
            if 'r_rp' in self.reg('r_ep'):
                self.reg('r_rp', self.reg('r_ep')['r_rp'])
            if 'r_rdp' in self.reg('r_ep'):
                self.reg('r_rdp', self.reg('r_ep')['r_rdp'])
            # put args in the env
            for k,v in zip(method["compiled_function"]["params"],args):
                self.env_set_value(k, v)

    def setup_and_run_unprotected(self, recv, drecv, name, method, args, should_allocate):
        self.exception_protection.append(False)
        logger.debug("unprotected: running fn with no protection -- " + str(self.exception_protection))
        try:
            res = self.setup_and_run_fun(recv, drecv, name, method, args, should_allocate)
            self.exception_protection.pop()
            return res
        except:
            logger.debug("unprotected: fn raised: popping and reraise -- " + str(self.exception_protection))
            self.exception_protection.pop()
            raise

    # def setup_and_run_protected(self, recv, drecv, name, method, args, should_allocate):
    #     self.exception_protection.append(True)
    #     logger.debug("protected: running fn with protection -- " + str(self.exception_protection))
    #     try:
    #         return (None, self.setup_and_run_fun(recv, drecv, name, method, args, should_allocate))
    #     except MemetalkException as e:
    #         logger.debug("protected: fn raised: popping and returning ex message -- " + str(self.exception_protection))
    #         self.exception_protection.pop()
    #         return (e.mmobj(), None)

    def do_send(self, receiver, selector, args):
        drecv, method = self._lookup(receiver, self.interpreter.get_vt(receiver), selector)
        if not method:
            self.throw_with_message("DoesNotUnderstand: " + selector + " -- " + P(receiver,1,True))
        else:
            return self.setup_and_run_fun(receiver, drecv, selector, method, args, True)

    ## env auxiliary
    def env_lookup(self, name):
        #the idx of name in self.reg('r_ep') or None
        if self.reg('r_ep') == None:
            return None
        def lookup(cfun, name):
            if name in cfun["env_table"].values():
                return [k for k,v in cfun["env_table"].iteritems() if v == name][0]
            elif cfun["outer_cfun"]:
                return lookup(cfun["outer_cfun"], name)
            else:
                return None
        return lookup(self.reg('r_cp')["compiled_function"], name)

    def env_set_value(self, name, value):
        idx = self.env_lookup(name)
        if idx != None:
            self.reg('r_ep')[idx] = value
        else:
            self.throw_with_message('Undeclared env variable: ' + name)

    def set_local_value(self, name, expr):
        if self.reg('r_ep') != None:
            self.env_set_value(name, expr)
        else:
            self.locals()[name] = expr

    ##### eval routines

    def do_eval(self, name, *rest):
        try:
            return getattr(self, name)(*rest)
        except StandardError as e:

            print "WARNING: python exception ocurred: " + str(e.__class__) + ":" + str(e)
            print ''.join(traceback.format_exc())

            if self.state == 'exception': # we know, let it propagate
                raise
            else:
                self.throw_py_exception(e, traceback.format_exc())

    def eval_prim(self, prim_name, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_prim')
        return globals()['prim_'+prim_name](self)

    def eval_do_field_assign(self, field, rhs, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_field_assign')

        if not field in self.reg('r_rdp'):
            self.throw_with_message("object has no field " + field)
        else:
            self.reg('r_rdp')[field] = rhs

    def eval_do_local_assign(self, name,expr, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_local_assign')
        self.set_local_value(name, expr)

    def eval_do_var_def(self, name, expr, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_var_def')
        self.set_local_value(name, expr)

    def eval_do_fun_lit(self, params, body, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_fun_lit')

        compiled_fun = body.cfun_literal
        ctx = self.interpreter.compiled_function_to_context(compiled_fun, self.reg('r_ep'), self.reg('r_mp'))
        return ctx

    def eval_access_field(self, field):
        if field in self.reg('r_rdp'):
            return self.reg('r_rdp')[field]
        else:
            self.throw_with_message("object has no field " + field)

    def eval_do_access_index(self, left, idx, ast):
        return left[idx]

    def eval_access_module(self):
        return self.reg('r_mp')

    def eval_access_context(self):
        return self.reg('r_cp')

    def eval_access_this(self):
        return self.reg('r_rp')

    def eval_symbol(self, s):
        return self.interpreter.interned_symbol_for(s)

    def eval_access_var(self, name, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_access_var')
        #-local
        if self.reg('r_ep') != None: # env
            idx = self.env_lookup(name)
            if idx != None:
                return self.reg('r_ep')[idx]
        else: # checking stack
            if name in self.locals():
                return self.locals()[name]

        #-module params + module entries
        return self.lookup_in_modules(name, self.reg('r_mp'))

    def lookup_in_modules(self, name, mp):
        if name in mp:
            return mp[name]
        elif mp['_delegate'] != None:
            return self.lookup_in_modules(name, mp['_delegate'])
        else:
            self.throw_with_message("Undeclared: " + name + " : "+P(self.reg('r_cp')['compiled_function'],1,True))

    def eval_do_return(self, value, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_return')
        raise ReturnException(value)

    def eval_do_super_send(self, args, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_super_send')

        instance = self.reg('r_rdp')
        klass = self.interpreter.get_vt(instance)
        pklass = klass["parent"]
        receiver = instance["_delegate"]

        selector = self.reg('r_cp')['compiled_function']['name']
        drecv, method = self._lookup(receiver, pklass, selector)

        if not method:
            self.throw_with_message("DoesNotUnderstand: " + selector + " -- " + P(instance,1,True))
        else:
            return self.setup_and_run_fun(self.reg('r_rp'), drecv, selector, method, args, False)

    def eval_do_super_ctor_send(self, selector, args, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_super_ctor_send')
        # -super.foo() can only be used inside constructors.
        # -normal methods can invoke super() to get its superclass version.
        # -A method m() cannot invoke a super method of different name.
        if not self.reg('r_cp')["compiled_function"]["is_ctor"]:
            self.throw_with_message("Cannot use super.m() outside ctor");

        instance = self.reg('r_rdp')
        klass = self.interpreter.get_vt(instance)
        pklass = klass["parent"]
        receiver = instance["_delegate"]

        drecv, method = self._lookup(receiver, self.interpreter.get_vt(pklass), selector)

        if not method:
            self.throw_with_message("DoesNotUnderstand: " + selector + " -- " + P(instance,1,True))
        elif not method["compiled_function"]["is_ctor"]:
            self.hrow_with_message("Method is not constructor: " + selector)
        else:
            return self.setup_and_run_fun(self.reg('r_rp'), drecv, selector, method, args, False)

    def eval_do_bin_send(self, selector, receiver, arg, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_bin_send')
        drecv, method = self._lookup(receiver, self.interpreter.get_vt(receiver), selector)
        if not method:
            self.throw_with_message("DoesNotUnderstand: " + selector + " -- " + P(receiver,1,True))
        else:
            return self.setup_and_run_fun(receiver, drecv, selector, method, [arg], True)

    def eval_do_un_not(self, value, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_un_not')
        return not value

    def eval_do_and(self, l, r, ast):
        return l and r

    def eval_do_un_neg(self, value, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_un_neg')
        return - value

    def eval_do_send(self, receiver, selector, args, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_send')
        return self.do_send(receiver, selector, args)

    def eval_do_call(self, fun, args, ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_call')
        return self.setup_and_run_fun(self.reg('r_rp'), self.reg('r_rdp'), '<?>', fun, args, True)


    def eval_do_send_or_call(self, name, args,ast):
        self.reg('r_ip', ast)
        self.dbg_control('eval_do_send_or_call')
        fn = None
        # if name is local...
        if self.reg('r_ep'):
            idx = self.env_lookup(name) # in env?
            if idx != None:
                fn = self.reg('r_ep')[idx]
        elif name in self.locals():        # local in stack
            fn = self.locals()[name]

        if fn:
            return self.setup_and_run_fun(self.reg('r_rp'), self.reg('r_rdp'), name, fn, args, True)
        fn = self.lookup_in_modules(name, self.reg('r_mp'))
        if fn:
            return self.setup_and_run_fun(self.reg('r_mp'), self.reg('r_mp'), name, fn, args, True)

    def eval_do_if(self, cond, yes):
        if cond: #False/None vs. *
            self.evaluator.apply("exprlist", yes)

    def eval_do_if_else(self, cond, yes, no):
        if cond: #False/None vs. *
            self.evaluator.apply("exprlist", yes)
        else:
            self.evaluator.apply("exprlist", no)

    def eval_do_while(self, cond_expr, yes_expr, ast):
        while True:
            cond_res = self.evaluator.apply("expr", cond_expr)[0]
            if cond_res == False:
                break
            self.evaluator.apply("exprlist", yes_expr)

    def eval_do_try(self, ast, tr, bind, ct):
        self.reg('r_ip', ast)
        try:
            self.exception_protection.append(True)
            ev = Eval([tr])
            ev.i = self
            return ev.apply("exprlist")[0]
        except MemetalkException as e:
            self.exception_protection.pop()
            self.set_local_value(bind, e.mmobj())
            return self.evaluator.apply("exprlist", ct)[0]

    def eval_do_debug(self,ast):
        self.reg('r_ip', ast)
        self.state = 'paused'

    def dbg_control(self, name):

        def process_breakpoints():
            for idx, bp in enumerate(self.volatile_breakpoints):
                line = self.reg('r_ip').start_line - self.reg('r_cp')['compiled_function']['line']+1
                logger.debug("Checking line: " + str(line) + " --- " + str(self.reg('r_ip')))
                if line == bp['line'] and id_eq(self.reg('r_cp')['compiled_function'], bp['cfun']):
                    logger.debug("BP activated at line: " + str(line) + " --- " + str(self.reg('r_ip')))
                    self.state = 'paused'
                    del self.volatile_breakpoints[idx]
                    return

        def process_dbg_msg(msg):
            logger.debug(str(self.procid) + ': received cmd: ' + P(msg,1,True))
            if msg['cmd'] == 'dbg_break':
                self.state = msg['value']
                logger.debug(str(self.procid) + " dbg_control::dbg_break set state to paused")
                logger.debug(str(self.procid) + " dbg_control::dbg_break SEND 'done dbg_break'")
                ipc.put(self.channels['dbg'], 'done dbg_break')
                return False
            if msg['cmd'] == 'set_state':
                self.state = msg['value']
                if self.state == 'running':
                    return True
                return False
            if msg['cmd'] == 'get_frames':
                logger.debug(str(self.procid) + ' dbg_control::get_frames: SENDing frames')
                ipc.put(self.channels['dbg'], self.all_stack_frames())
                logger.debug('frames sent, we are done')
                return False
            if msg['cmd'] == 'step_into':
                self.state = 'paused'
                return True
            if msg['cmd'] == 'step_over':
                self.state = 'next'
                return True
            if msg['cmd'] == 'continue':
                logger.debug('target: received continue')
                self.state = 'running'
                return True
            if msg['cmd'] == 'eval_in_frame':
                text = msg['args'][0]
                frame_index = msg['args'][1]

                old_state = self.state
                self.state = 'running'
                try:
                    self.exception_protection.append(True) #run in protected mode
                    fn = _context_with_frame(self, text, self.all_stack_frames()[frame_index])
                    val = self.setup_and_run_fun(None, None, fn['compiled_function']['name'], fn, [], True)
                    res = (None, val)
                except MemetalkException as e:
                    res = (e.mmobj(), None)
                self.exception_protection.pop()
                logger.debug(str(self.procid) + ' dbg_control::eval_in_frame: SENDing result')
                ipc.put(self.channels['dbg'],  res)
                self.state = old_state
                return False
            if msg['cmd'] == 'reload_frame':
                logger.debug(str(self.procid) + ': reloading frame')
                self.state = 'paused'
                self.last_exception = None
                raise RewindException(1)
            if msg['cmd'] == 'rewind_and_break':
                logger.debug('target: rewind_and_break')

                self.state = 'running'
                self.last_exception = None

                frames_count = msg['args'][0]
                logger.debug('rewinding  this much of frames: ' + str(frames_count))
                logger.debug('We are at: ' + self.reg('r_cp')['compiled_function']['name'])

                to_line = msg['args'][1]
                logger.debug('line: ' + str(to_line))

                logger.debug('we will break at: ' + self.reg('r_cp')['compiled_function']['name'])
                self.break_at(self.reg('r_cp')['compiled_function'], to_line)
                raise RewindException(frames_count)
            if msg['cmd'] == 'update_object':
                logger.debug(str(self.procid) + ": updating our version of object " + str(msg["value"]["@id"]))
                self.interpreter.object_become(msg['value'])
            if msg['cmd'] == 'update_cmodule':
                cmod = msg['value']
                logger.debug(str(self.procid) + ": updating our version of cmodule " + str(cmod["@id"]))
                self.interpreter.compiled_modules[cmod['name']] = cmod
        process_breakpoints()

        # intercepting while we are running:
        if self.state == 'running' and not self.channels['my'].empty():
            logger.debug(str(self.procid) + ": cmd intercepted. WAIT..")
            msg = ipc.get(self.channels['my'])
            logger.debug(str(self.procid) + ": cmd intercepted. RECV..")
            process_dbg_msg(msg)


        if self.state in ['paused', 'next', 'exception']:
            logger.debug(str(self.procid) + ": dbg_control paused! state: " + self.state)
            logger.debug(str(self.procid) + ": SEND frames to debugger")
            ipc.put(self.channels['dbg'], self.all_stack_frames())
            logger.debug(str(self.procid) + ": dbg_control entering cmd loop")
            while True:
                logger.debug(str(self.procid) + ': WAIT for debugger command')
                msg = ipc.get(self.channels['my'])
                logger.debug(str(self.procid) + ': RECV command')
                if process_dbg_msg(msg):
                    logger.debug(str(self.procid) + ": dbg_control resuming execution")
                    break

        return self.last_exception != None


    def is_exception_protected(self):
        logger.debug("is_exception_protected?: " + str(self.exception_protection[-1]))
        return self.exception_protection[-1]

#         if self.state == 'exception':
#             logger.debug(str(self.procid) + ": dbg_control in exception state")
#             self.debug_me()
#             logger.debug(str(self.procid) + ": dbg_control done")
# #            logger.debug(str(self.procid) + ": dbg_control: debug_me() done")
# #            process_dbg_msg({"cmd":"set_state","value":"paused"}) #so it sends done set_state to dbg

    def throw(self, mex):
        # MemetalkException encapsulates the memetalk exception:
        if not self.is_exception_protected() and self.flag_debug_on_exception:
            logger.debug(str(self.procid) + ": throw: in exception state")
            self.last_exception = mex
            self.debug_me('exception')
            if self.dbg_control("throw"):
                if 'py_exception' in mex and mex['py_exception'] != None:
                    raise mex['py_exception']
                else:
                    raise MemetalkException(mex, self.pp_stack_trace())
        else:
            if 'py_exception' in mex and mex['py_exception'] != None:
                raise mex['py_exception']
            else:
                raise MemetalkException(mex, self.pp_stack_trace())

    def throw_py_exception(self, pyex, tb):
        logger.debug("Python exception with message: \n" + str(pyex.message))
        logger.debug(tb)
        ex = self.interpreter.create_instance(self.interpreter.core_imod['Exception'])

        # ...and this me being stupid:
        ex['message'] =  "Python exception: " + str(pyex.message)

        if not self.is_exception_protected() and self.flag_debug_on_exception:
            logger.debug(str(self.procid) + ": throw_py: in exception state")
            self.last_exception = ex
            self.debug_me('exception')
            if self.dbg_control("throw"):
                raise MemetalkException(ex, self.pp_stack_trace(), pyex, tb)
        else:
            # MemetalkException encapsulates the memetalk exception:
            raise MemetalkException(ex, self.pp_stack_trace(), pyex, tb)

    def throw_with_message(self, msg):
        logger.debug("MemetalkException with message: \n" + msg)
        logger.debug(self.pp_stack_trace())
        ex = self.interpreter.create_instance(self.interpreter.core_imod['Exception'])

        # ...and this me being stupid:
        ex['message'] =  msg

        if not self.is_exception_protected() and self.flag_debug_on_exception:
            logger.debug(str(self.procid) + ": throw_py: in exception state")
            self.last_exception = ex
            self.debug_me('exception')
            if self.dbg_control("throw"):
                raise MemetalkException(ex, self.pp_stack_trace())
        else:
            # MemetalkException encapsulates the memetalk exception:
            raise MemetalkException(ex, self.pp_stack_trace())

    def pp_stack_trace(self):
        st =  "\n**** Memetalk stack trace *** :\n"
        for frame in self.all_stack_frames():
            if frame['r_cp']:
                st = st +  (frame['r_cp']['compiled_function']['name'] + " :::" + str(frame['r_cp']['compiled_function']['body']))[0:80] + "...\n"
        if self.reg('r_cp'):
            st = st + (self.reg('r_cp')['compiled_function']['name'] + " ::: " + str(self.reg('r_cp')['compiled_function']['body']))[0:80] + "...\n"
        return st

###################################################






if __name__ == "__main__":
    #sys.setrecursionlimit(10000000) # yes, really

    if len(sys.argv) == 1:
        print "i.py filename"
        sys.exit(0)

    if 'DEBUG' in os.environ:
        logger.setLevel(logging.DEBUG)
    logger.addHandler(logging.StreamHandler(sys.stdout))

    Interpreter().start(sys.argv[1])
