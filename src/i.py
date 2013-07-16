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

def P(obj, depth=1):
    if depth > 5:
        depth = None
    pprint(obj, None, 1, 80, depth)


def _create_compiled_module(data):
    template = {"_vt": core.CompiledModule,
                "_delegate": None,
                "name": "",
                "filepath":"",
                "params": [],
                "compiled_functions": {},
                "compiled_classes": {}}

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
                "env_table_skel": None,
                "outter_cfun":None,
                "owner":None,
                "is_ctor": False,
                "@tag": "a CompiledFunction"}
    return dict(template.items() + data.items())

def _create_compiled_class(data):
    template = {"_vt": core.CompiledClass,
                "_delegate": None,
                "name": "",
                "super_class_name":"",
                "fields": [],
                "methods": {},
                "own_methods":{}}
    return dict(template.items() + data.items())

def _create_module(data):
    template = {"_vt": core.ModuleBehavior,
                "_delegate": None,
                "parent": core.Object,
                "size": 1, #delegate
                "dict": {},
                "compiled_module": None}
    return dict(template.items() + data.items())

def _create_class(data):
    template = {"_vt": None, #it should be given by data [FooClassBehavior]
                "_delegate": None,
                "parent": None,
                "size": 1, #delegate
                "dict": {},
                "compiled_class":None}
    return dict(template.items() + data.items())


def _create_accessor_method(imodule, name):
    cf =  _create_compiled_function({
            "name": name,
            "body":  [['return', ['field', name]]]})
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


def _instantiate_module(compiled_module, args, parent_module):
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
    classes = {}
    bclasses = {}
    super_later = {}
    for _, c in compiled_module["compiled_classes"].items():
        super_name = c["super_class_name"]
        super_class = None
        if super_name in args.keys():
            super_class = args[super_name]
        elif super_name in parent_module:
            super_class = parent_module[super_name] # TODO: should be actually a msg send
        elif super_name in compiled_module["compiled_classes"]:
            super_later[c["name"]] = super_name
        else:
            raise Exception("super class not found:" + super_name)

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

class ASTBuilder:
    def ast(self, begin, ast):
        end = self.parser.input.position
        full = ''.join(self.parser.input.data)
        text = full[begin:end]
        line = full[:begin].count("\n") + self.line_offset
        node = ASTNode(ast, self.filename, text, line+1)
        return node

class ASTNode(list):
    def __init__(self, lst, filename, text, line):
        list.__init__(self,lst)
        self.filename = filename
        self.text = text
        self.line = line

class FunctionLoader(ASTBuilder): # for eval
    def load_fun(self, cfun, code):
        self.line_offset = cfun['line']
        self.filename = "?"
        self.parser = MemeParser(code)
        self.parser.i = self
        try:
            ast,err = self.parser.apply("top_level_fun")
            #print "--F AST"
            #print ast
            #print "//--F AST"
        except Exception as err:
            if hasattr(err,'formatError'):
                print(err.formatError(''.join(self.parser.input.data)))
            else:
                traceback.print_exc()
            raise err

        self.env_idx = 0
        self.env_id_table = []
        self.functions = []

        loader = Loader([ast])
        loader.i = self
        loader.apply("function_definition")

        for k,v in cfun.iteritems():
            cfun[k]  = self.functions[0][k]

        #P(cfun, 4)
        return cfun


    def load_body(self, code, params, owner, env):
        self.line_offset = 0
        self.filename = "<eval>"
        self.parser = MemeParser("{"+code+"}")
        self.parser.i = self
        try:
            ast,err = self.parser.apply("as_eval")
            print "--EVAL AST"
            print ast
            print "//--EVAL AST"
        except Exception as err:
            if hasattr(err,'formatError'):
                print(err.formatError(''.join(self.parser.input.data)))
            else:
                traceback.print_exc()
            raise err

        uses_env = env != {}

        # given env
        _env1 = dict(zip(range(0,len(env.keys())), env.keys()))
        self.env_idx = len(env.keys())

        # parameters env
        _env2 = dict(zip(range(self.env_idx,self.env_idx+len(params)),params))
        self.env_idx = self.env_idx+len(params)

        self.env_id_table = [dict(_env1.items() + _env2.items())]

        self.functions = []
        self.functions.append(_create_compiled_function({"name":"<anonymous>",
                                                         "outter_cfun": None,
                                                         "params": params,
                                                         "body": ast,
                                                         "text": code,
                                                         "line":1,
                                                         'env_table': self.env_id_table[0],
                                                         'uses_env': uses_env,
                                                         'env_table_skel': dict(zip(range(0,self.env_idx),[None]*self.env_idx)),
                                                         'owner': owner,
                                                         "@tag":"a compiled literal function"}))

        loader = Loader([ast])
        loader.i = self
        loader.apply("load_body")
        return self.functions[0]


    def l_begin_function(self, name, is_ctor):
        self.env_id_table.append({})
        self.functions.append(_create_compiled_function({"name":name,
                                                         "is_ctor": is_ctor,
                                                         "@tag":"a compiled function"}))

    def l_enter_literal_fun(self):
        self.env_id_table.append({})
        self.functions[0]["uses_env"] = True # root Function using env
        self.functions.append(
            _create_compiled_function({"name":"<anonymous>",
                                       "outter_cfun": self.functions[-1],
                                       "@tag":"a compiled literal function"}))

    def l_set_fun_literal_parameters(self, params):
        self.env_id_table[-1] = dict(zip(range(self.env_idx,self.env_idx+len(params)),params))
        self.env_idx = self.env_idx + len(params)

        self.functions[-1]["params"] = params

    def l_literal_fun_body(self, body):
        self.functions[-1]["body"] = body

    def l_done_literal_function(self, ast):
        function = self.functions.pop()
        function['text'] = ast.text
        function['line'] = ast.line

        body = function["body"]

        function['env_table'] = self.env_id_table.pop()

        #hack to identify the literal function when executing it
        #body[0]: the first instruction. The body changes id, unfortunately
        #literal is in the parent fun, so it can be easily fetched during execution
        self.functions[-1]["fun_literals"][id(body[0])] = function

    def l_var_def(self, name):
        self.env_id_table[-1][self.env_idx] = name
        self.env_idx = self.env_idx + 1


    def l_set_function_parameters(self, params):
        self.env_idx = 0
        self.env_id_table[-1] = dict(zip(range(self.env_idx,self.env_idx+len(params)),params))
        self.env_idx = len(params)
        self.functions[-1]["params"] = params

    def l_end_function(self, body,ast):
        function = self.functions[-1]
        function['body'] = body
        function['text'] = ast.text
        function['line'] = ast.line

        env_table = self.env_id_table.pop()
        if function["uses_env"]:
            function['env_table'] = env_table
            function['env_table_skel'] =  dict(zip(range(0,self.env_idx),[None]*self.env_idx))

class ModuleLoader(ASTBuilder):
    def compile_module(self, i, filename):
        self.line_offset = 0
        self.pos_stack = []
        #self.function_line_offset = 0
        self.filename = filename

        src = i.open_module_file(filename).read()
        self.parser = MemeParser(src)
        self.parser.i = self
        try:
            ast,_ = self.parser.apply("start")
        except Exception as err:
            if hasattr(err,'formatError'):
                print(err.formatError(''.join(self.parser.input.data)))
            else:
                traceback.print_exc()
            sys.exit(1)

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

        self.loading_class = False

        loader = Loader([ast])
        loader.i = self
        loader.apply("load_module")
        return self.current_module

    def l_module(self, name, p):
        self.current_module["name"] = name
        self.current_module["params"] = p

    def l_begin_class(self, name, super_class, fields):
        self.loading_class = True
        self.current_class = _create_compiled_class({"name":name,
                                                     "super_class_name":super_class,
                                                     "fields":fields})

    def l_end_class(self):
        self.loading_class = False
        cname = self.current_class["name"]
        self.current_module["compiled_classes"][cname] = self.current_class
        self.current_class = None

    def l_begin_function(self, name, is_ctor):
        self.env_id_table.append({})
        self.functions.append(_create_compiled_function({"name":name,
                                                         "is_ctor": is_ctor,
                                                         "@tag":"a compiled function"}))

    def l_var_def(self, name):
        self.env_id_table[-1][self.env_idx] = name
        self.env_idx = self.env_idx + 1

    def l_set_function_parameters(self, params):
        self.env_idx = 0
        self.env_id_table[-1] = dict(zip(range(self.env_idx,self.env_idx+len(params)),params))
        self.env_idx = len(params)
        self.functions[-1]["params"] = params

    def l_end_function(self, body,ast):
        function = self.functions.pop()
        function['body'] = body
        function['text'] = ast.text
        function['line'] = ast.line

        env_table = self.env_id_table.pop()
        if function["uses_env"]:
            function['env_table'] = env_table
            function['env_table_skel'] =  dict(zip(range(0,self.env_idx),[None]*self.env_idx))

        fname = function["name"]
        if self.loading_class:
            if function["is_ctor"]:
                self.current_class["own_methods"][fname] = function
            else:
                self.current_class["methods"][fname] = function
            function['owner'] = self.current_class
        else:
            self.current_module["compiled_functions"][fname] = function
            function['owner'] = self.current_module


    def l_enter_literal_fun(self):
        self.env_id_table.append({})
        self.functions[0]["uses_env"] = True # root Function using env
        self.functions.append(
            _create_compiled_function({"name":"<anonymous>",
                                       "outter_cfun": self.functions[-1],
                                       "@tag":"a compiled literal function"}))

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
        function['line'] = ast.line

        function['env_table'] = self.env_id_table.pop()

        #hack to identify the literal function when executing it
        #body[0]: the first instruction. The body changes id, unfortunately
        #literal is in the parent fun, so it can be easily fetched during execution
        self.functions[-1]["fun_literals"][id(body[0])] = function


#######################################################
## Executing
#######################################################

class ReturnException(Exception):
    def __init__(self, val):
        self.val = val

class RewindException(Exception):
    def __init__(self, count):
        self.count = count

class Interpreter():
    def __init__(self):
        self.compiled_modules = {}
        self.processes = []

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
        self.compiled_modules[filename] = self.compile_module(filename)
        compiled_module = self.compiled_modules[filename]

        imodule = _instantiate_module(compiled_module, {}, core.kernel_imodule)
        process = Process(self)
        self.processes.append(process)
        process.switch('run_module', imodule, [])

    def debug_process(self, target_process):
        compiled_module = self.compile_module('debugger-entry.mm')
        imodule = _instantiate_module(compiled_module, {}, core.kernel_imodule)
        target_process.debugger_process = Process(self)
        self.processes.append(target_process.debugger_process)

        mmprocess = self.alloc(core.VMProcess, {'self':target_process})
        return target_process.debugger_process.switch('run_module', imodule, [mmprocess])

    def compile_module(self, filename):
        return ModuleLoader().compile_module(self,filename)

    def instantiate_module(self, compiled_module, args, imodule):
        return _instantiate_module(compiled_module, args, imodule)

    def kernel_module_instance(self):
        return core.kernel_imodule

    def get_class(self, name):
        return getattr(core, name)

    def create_compiled_function(self, data):
        return _create_compiled_function(data)

    def create_function_from_cfunction(self, cfun, imodule):
        return _function_from_cfunction(cfun, imodule)

    def compiled_function_to_context(self, cfun, env, imodule):
        return _compiled_function_to_context(cfun, env, imodule)

    def compile_code(self, text, params, owner, env):
        return FunctionLoader().load_body(text, params, owner, env)

    def recompile_fun(self, cfun, code):
        return FunctionLoader().load_fun(cfun, code)

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

class Process(greenlet):
    def __init__(self, interpreter):
        super(Process, self).__init__(self.greenlet_entry)

        self.interpreter = interpreter
        self.dbg_proc = None
        self.state = None

    def greenlet_entry(self, cmd, *rest):
        getattr(self, cmd)(*rest)

    def run_module(self, module, args):
        fun = module["main"]

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

        ret = self.setup_and_run_fun(module, module, 'main', module['main'], args, True)
        print "RETVAL: " + str(ret)

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
            self.r_rp = prim_basic_new(self)
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
                ret,err = self.evaluator.apply("exec_fun")
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
            self.tear_fun()
            if skip:
                self.state = 'paused'
            return ret
        return evaluate(self.state == 'next')

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
        if type(method) == str:
            traceback.print_exc()
            #print("function '" + name + "' lacks implementation. Receiver:");
            #P(recv)
            sys.exit(1)
        if len(method["compiled_function"]["params"]) != len(args):
            #P(method,3)
            raise Exception("arity error: " + method['compiled_function']['name'] +\
                                ", expecting " + str(len(method["compiled_function"]["params"])) +\
                                ", got " + str(len(args)) + " -- "+pformat(args,1,80,2))

        #backup frame
        self.stack.append({
                "r_cp": self.r_cp,
                "r_rp": self.r_rp,
                "r_rdp": self.r_rdp,
                "r_ep" : self.r_ep,
                'r_ip' : self.r_ip,
                'locals':self.locals})
                # no need to backup mp, it can be infered from current fun

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
            elif cfun["outter_cfun"]:
                return lookup(cfun["outter_cfun"], name)
            else:
                return None
        return lookup(self.r_cp["compiled_function"], name)

    def env_set_value(self, name, value):
        idx = self.env_lookup(name)
        if idx != None:
            self.r_ep[idx] = value

    ##### eval routines

    def eval_prim(self, prim_name):
        try:
            return globals()['prim_'+prim_name](self)
        except KeyError as e:
            traceback.print_exc()
            sys.exit(1)

    def eval_do_field_attr(self, field, rhs, ast):
        self.r_ip = ast
        self.dbg_control()

        if not field in self.r_rdp:
            raise Exception("object has no field " + field)
        else:
            self.r_rdp[field] = rhs

    def eval_do_local_attr(self, name,expr, ast):
        self.r_ip = ast
        self.dbg_control()

        if self.r_ep != None:
            self.env_set_value(name, expr)
        elif name in self.locals:
            self.locals[name] = expr
        else:
            raise Exception("Undeclared variable: " + name)

    def eval_do_var_def(self, name, expr):
        if self.r_ep != None:
            self.env_set_value(name, expr)
        else:
            self.locals[name] = expr

    def eval_do_fun_lit(self, params, body):
        compiled_fun = self.r_cp["compiled_function"]["fun_literals"][id(body[0])]
        return _compiled_function_to_context(compiled_fun, self.r_ep, self.r_mp)

    def eval_access_field(self, field):
        if field in self.r_rdp:
            return self.r_rdp[field]
        else:
            raise Exception("object has no field " + field)

    def eval_do_access_index(self, left, idx, ast):
        return left[idx]

    def eval_access_module(self):
        return self.r_mp

    def eval_access_this(self):
        return self.r_rp

    def eval_access_var(self, name):
        #-local
        if self.r_ep != None: # env
            idx = self.env_lookup(name)
            if idx != None:
                return self.r_ep[idx]
        else: # checking stack
            if name in self.locals:
                return self.locals[name]

        #-module params + module entries
        if name in self.r_mp:
            return self.r_mp[name]
        else:
            raise Exception("Undeclared var: " + name + " : "+pformat(self.r_cp['compiled_function'],1,80,1))

    def eval_do_return(self, value, ast):
        self.r_ip = ast
        self.dbg_control()
        raise ReturnException(value)

    def eval_do_super_ctor_send(self, selector, args):
        # -super.foo() can only be used inside constructors.
        # -normal methods can invoke super() to get its superclass version.
        # -A method m() cannot invoke a super method of different name.
        if not self.r_cp["compiled_function"]["is_ctor"]:
            raise Exception("Cannot use super.m() outside ctor");

        instance = self.r_rdp
        klass = self.interpreter.get_vt(instance)
        pklass = klass["parent"]
        receiver = instance["_delegate"]

        drecv, method = self._lookup(receiver, self.interpreter.get_vt(pklass), selector)

        if not method:
            traceback.print_exc()
            print("DoesNotUnderstand: " + selector + " -- " + pformat(instance,1,80,1))
            sys.exit(1)
        elif not method["compiled_function"]["is_ctor"]:
            raise Exception("Method is not constructor: " + selector)
        else:
            return self.setup_and_run_fun(receiver, drecv, selector, method, args, False)

    def eval_do_bin_send(self, selector, receiver, arg, ast):
        self.r_ip = ast
        #self.dbg_control()
        drecv, method = self._lookup(receiver, self.interpreter.get_vt(receiver), selector)
        if not method:
            traceback.print_exc()
            print("DoesNotUnderstand: " + selector + " -- " + pformat(receiver,1,80,1))
            sys.exit(1)
        else:
            return self.setup_and_run_fun(receiver, drecv, selector, method, [arg], True)

    def eval_do_un_not(self, value, ast):
        self.r_ip = ast
        self.dbg_control()
        return not value

    def eval_do_send(self, receiver, selector, args, ast):
        self.r_ip = ast
        self.dbg_control()
        drecv, method = self._lookup(receiver, self.interpreter.get_vt(receiver), selector)
        if not method:
            traceback.print_exc()
            print("DoesNotUnderstand: " + selector + " -- " + pformat(receiver,1,80,1))
            sys.exit(1)

        else:
            return self.setup_and_run_fun(receiver, drecv, selector, method, args, True)

    def eval_do_call(self, fun, args, ast):
        self.r_ip = ast
        self.dbg_control()
        return self.setup_and_run_fun(self.r_rp, self.r_rdp, '<?>', fun, args, True)


    def eval_do_send_or_call(self, name, args,ast):
        self.r_ip = ast
        self.dbg_control()
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

        if name in self.r_mp:            # shared
            fn = self.r_mp[name]
            return self.setup_and_run_fun(self.r_mp, self.r_mp, name, fn, args, True)

        raise Exception("Undeclared function: " + name)

    def eval_do_if(self, cond, yes):
        if cond: #False/None vs. *
            self.evaluator.apply("exprlist", yes)

    def eval_do_if_else(self, cond, yes, no):
        if cond: #False/None vs. *
            self.evaluator.apply("exprlist", yes)
        else:
            self.evaluator.apply("exprlist", no)

    def eval_do_debug(self,ast):
        self.r_ip = ast
        self.dbg_cmd(self.interpreter.debug_process(self))

    ## debugger

    def dbg_cmd(self, cmd):
        if cmd == 'step_into':
            #print("process:: changed state to paused")
            self.state = 'paused'
        if cmd == 'step_over':
            #print("process:: changed state to next")
            self.state = 'next'
        if cmd == 'continue':
            #print("process:: changed state to continue")
            self.state = 'running'
        if cmd == 'rewind':
            #print("process: rewinding 1")
            raise RewindException(1)

    def dbg_control(self):
        if self.state == 'paused' or self.state == 'next':
            #print("switching back to debugger")
            self.dbg_cmd(self.debugger_process.switch())

if __name__ == "__main__":
    if len(sys.argv) == 1:
        print "i.py filename"
        sys.exit(0)
    Interpreter().start(sys.argv[1])
