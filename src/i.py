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
from parser import MemeParser
from loader import Loader
from evaluator import Eval
from prim import *
from pprint import pprint, pformat
from pdb import set_trace as br

def P(obj, depth=1):
    if depth > 5:
        depth = None
    pprint(obj, None, 1, 80, depth)

Behavior = {"_vt":"*replace-me*", # with myself
            "parent": None,
            "dict": {"toSource":"*replace-me*"},
            "@tag":"Behavior"}
Behavior["_vt"] = Behavior

ObjectBehavior = {"_vt": Behavior,
                  "parent":None,
                  "dict":{"new": "*replace-me", #with the 'new' method
                          "toString":"*replace-me*", #with the 'toString' method
                          "toSource":"*replace-me*"}, #with toSource
                  "@tag":"ObjectBehavior"}

Object = {"_vt": ObjectBehavior,
          "parent":None,
          "size":0,
          "dict": {"toString":"*replace-me", "toSource":"*replace-me*",
                   "==":"*replace-me*",
                   "!=":"*replace-me*"},
          "compiled_class": {"_vt": "*replace-me*",       # with CompiledClass
                             "_delegate": None,
                             "name": "Object",
                             "super_class_name":"",
                             "fields": [],
                             "methods": {},
                             "own_methods":{}}, #actually it has new
          "@tag": "Object"}


CompiledClassBehavior = {"_vt": Behavior,
                         "parent": ObjectBehavior,
                         "dict": {},
                         "@tag": "CompiledClassBehavior"}


CompiledClass = {"_vt": CompiledClassBehavior,
                 "_delegate": None,
                 "parent": Object,
                 "size": 5,
                 "dict": {}, #name,superclass, fields,methods
                 "@tag": "CompiledClass"}

Object["compiled_class"]["_vt"] = CompiledClass

CompiledFunctionBehavior = {"_vt": Behavior,
                            "parent": ObjectBehavior,
                            "dict": {'new':"*replace-me*"}, # with its prim new
                            "@tag": "CompiledFunctionBehavior"}

CompiledFunction_CompiledClass = {"_vt": CompiledClass,
                                       "_delegate": None,
                                       "name": "CompiledFunction",
                                       "super_class_name":"Object",
                                       "fields": [],
                                       "methods": {},  #actually it has new
                                       "own_methods":{}}

CompiledFunction = {"_vt":CompiledFunctionBehavior,
                    "_delegate": None,
                    "parent": Object,
                    "size": 13, #delegate, body, env_table, env_table_skel, fun_literals, is_ctor, is_prim, name, params, prim_name, uses_env, outter_cfun, owner
                    "dict": {"asContext":"*replace-me*"}, # with the function prim_cfun_as_context
                    "compiled_class": CompiledFunction_CompiledClass,
                    "@tag":"CompiledFunction"}


FunctionBehavior = {"_vt": Behavior,
                    "parent": ObjectBehavior,
                    "dict": {},
                    "@tag": "FunctionBehavior"}

Function = {"_vt": FunctionBehavior,
            "_delegate": Object,
            "parent": Object,
            "dict": {},
            "size": 3, #compiled_function, module, delegate
            "@tag": "Function"}

ContextBehavior = {"_vt":Behavior,
                   "parent": ObjectBehavior,
                   "dict": {},
                   "@tag": "ContextBehavior"}

Context = {"_vt": ContextBehavior,
           "_delegate": Object,
           "parent": Object,
           "dict": {'apply': "*replace-me*",
                    "getEnv": '*replace-me*'},
           "size": 4} # delegate, compiled_fun, module, env

CompiledModuleBehavior = {"_vt": Behavior,
                          "parent": ObjectBehavior,
                          "dict": {},
                          "@tag": "CompiledModuleBehavior"}

CompiledModule = {"_vt": CompiledModuleBehavior,
                  "_delegate": None,
                  "parent": Object,
                  "size": 7, #name, filepath, params, ast, funs, classes
                  "dict": {},
                  "@tag": "CompiledModule"}

ModuleBehavior = {"_vt": Behavior,
                  "parent": ObjectBehavior,
                  "dict": {"compiled_module": "*replace-me*"}, #with the getter
                  "@tag": "ModuleBehavior"}

KernelModule = {"_vt": ModuleBehavior,
                "_delegate": None,
                "parent": Object,
                "size": 2, #delegate, Object
                "dict": {"Object": "*replace-me*"}, #with the getter
                "@tag": "KernelModule"}

# kernel_module_instance_template = {"_vt": KernelModule,
#                                    "Object": Object} #Array,String, Number...

## Basic types

StringBehavior = {"_vt": Behavior,
                  "parent": ObjectBehavior,
                  "dict": {},
                  "@tag": "StringBehavior"}

String = {"_vt": StringBehavior,
          "_delegate": None,
          "parent": Object,
          "size": 0,
          "dict": {'size': '*replace-me*'},
          "@tag": "String"}

DictionaryBehavior = {"_vt": Behavior,
                      "parent": ObjectBehavior,
                      "dict": {},
                      "@tag": "DictionaryBehavior"}


Dictionary = {"_vt": DictionaryBehavior,
              "_delegate": None,
              "parent": Object,
              "size": 0,
              "dict": {"+": "*replace-me*"},
              "@tag": "Dictionary"}


ListBehavior = {"_vt": Behavior,
                      "parent": ObjectBehavior,
                      "dict": {},
                      "@tag": "ListBehavior"}


List = {"_vt": ListBehavior,
        "_delegate": None,
        "parent": Object,
        "size": 0,
        "dict": {"each": "*replace-me*"},
        "@tag": "List"}

NumberBehavior = {"_vt": Behavior,
                  "parent": ObjectBehavior,
                  "dict": {},
                  "@tag": "NumberBehavior"}

Number = {"_vt": NumberBehavior,
          "_delegate": None,
          "parent":Object,
          "size": 0,
          "dict": {"+": "*replace-me*"},
          "@tag": "Number"}

MirrorBehavior = {"_vt": Behavior,
                  "parent": ObjectBehavior,
                  'dict':{'new':'*replace-me*'},
                  "@tag": "MirrorBehavior"}


Mirror = {"_vt": MirrorBehavior,
          "_delegate": None,
          "compiled_class": {"_vt": CompiledClass,
                             "_delegate": None,
                             "name": "Mirror",
                             "super_class_name":"Object",
                             "fields": ['mirrored'],
                             "methods": {},
                             "own_methods":{}},
          "parent": Object,
          "size": 0,
          "dict": {'fields': '*replace-me*',
                   'valueFor': '*replace-me*',
                   'setValueFor':'*replace-me*'},
          "@tag": "Mirror"}

##############################################



def _create_compiled_module(data):
    template = {"_vt":CompiledModule,
                "_delegate": None,
                "name": "",
                "filepath":"",
                "params": [],
                "compiled_functions": {},
                "compiled_classes": {}}

    return dict(template.items() + data.items())

def _create_compiled_function(data):
    template = {"_vt": CompiledFunction,
                "_delegate": None,
                "name": "",
                "params": [],
                "body": None,
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
    template = {"_vt": CompiledClass,
                "_delegate": None,
                "name": "",
                "super_class_name":"",
                "fields": [],
                "methods": {},
                "own_methods":{}}
    return dict(template.items() + data.items())

def _create_module(data):
    template = {"_vt": ModuleBehavior,
                "_delegate": None,
                "parent": Object,
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

def _create_kernel_module_instance():
    return {"_vt": KernelModule,
            "_delegate": None,
            "Object": Object,
            "@tag":"kernel module instance"} #etc...

def _create_accessor_method(imodule, name):
    cf =  _create_compiled_function({
            "name": name,
            "body":  [['return', ['field', name]]]})
    return _function_from_cfunction(cf, imodule)

def _function_from_cfunction(cfun, imodule):
    return {"_vt": Function,
            "_delegate": None,
            "compiled_function": cfun,
            "module": imodule,
            "@tag": "a Function"}

def _compiled_function_to_context(cfun, env, imodule):
    return {"_vt": Context,
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


_kernel_imodule = _create_kernel_module_instance()
_Object_new_compiled_fun = _create_compiled_function({
        "name": "new",
        "body": [["return-this"]],
        "is_ctor": True,
        "owner": Object,
        "@tag": "Object.new compiled function"})

ObjectBehavior["dict"]["new"] = _function_from_cfunction(_Object_new_compiled_fun, _kernel_imodule)

_CompiledFunction_new_compiled_fun = _create_compiled_function({
    "name": "new",
    "params": ['text', 'parameters', 'module', 'env'],
    "body": ["primitive", ['literal-string', "compiled_function_new"]],
    "is_ctor": True,
    "owner": CompiledFunction_CompiledClass, #the CompiledClass for CompiledFunction class
    "@tag": "CompiledFunction.new compiled function"})

CompiledFunctionBehavior["dict"]["new"] = _function_from_cfunction(_CompiledFunction_new_compiled_fun, _kernel_imodule)

CompiledFunction['dict']['asContext'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "asContext",
            "params": ['imodule', 'self', 'env'],
            "body": ["primitive", ['literal-string', "compiled_function_as_context"]],
            "is_ctor": False,
            #"owner": CompiledFunction_CompiledClass, #the CompiledClass for CompiledFunction class. So far, only used on ctor/rdp lookup
            "@tag": "<CompiledFunction>.asContext compiled function"}),
    _kernel_imodule)

Context['dict']['apply'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "apply",
            "params": ['args'],
            "body": ["primitive", ['literal-string', "context_apply"]],
            "is_ctor": False,
            #"owner": Context_CompiledClass, #the CompiledContextClass for Context. So far, only used on ctors/rdp lookup
            "@tag": "<Function>.apply compiled function"}),
    _kernel_imodule)

Context['dict']['getEnv'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "getEnv",
            "params": [],
            "body": ["primitive", ['literal-string', "context_get_env"]],
            "is_ctor": False,
            #"owner": Context_CompiledClass, #the CompiledContextClass for Context. So far, only used on ctors/rdp lookup
            "@tag": "<Function>.getEnv compiled function"}),
    _kernel_imodule)

String['dict']['size'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "replace",
            "params": [],
            "body": ["primitive", ['literal-string', "string_size"]],
            "is_ctor": False,
#            "owner": CompiledFunction_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<String>.size compiled function"}),
    _kernel_imodule)

Object['dict']['toString'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "toString",
            "params": [],
            "body": ["primitive", ['literal-string', "object_to_string"]],
            "is_ctor": False,
#            "owner": Number_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Object>.toString compiled function"}),
    _kernel_imodule)

ObjectBehavior['dict']['toString'] = Object['dict']['toString'] #puff
Behavior['dict']['toSource'] = Object['dict']['toString'] #puff

Object['dict']['toSource'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "toSource",
            "params": [],
            "body": ["primitive", ['literal-string', "object_to_source"]],
            "is_ctor": False,
#            "owner": Number_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Object>.toSource compiled function"}),
    _kernel_imodule)

ObjectBehavior['dict']['toSource'] = Object['dict']['toSource'] #puff

Object['dict']['=='] = _function_from_cfunction(
    _create_compiled_function({
            "name": "==",
            "params": ['other'],
            "body": ["primitive", ['literal-string', "object_equal"]],
            "is_ctor": False,
#            "owner": Number_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Object>.== compiled function"}),
    _kernel_imodule)

Object['dict']['!='] = _function_from_cfunction(
    _create_compiled_function({
            "name": "==",
            "params": ['other'],
            "body": ["primitive", ['literal-string', "object_not_equal"]],
            "is_ctor": False,
#            "owner": Number_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Object>.== compiled function"}),
    _kernel_imodule)

Number['dict']['+'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "+",
            "params": ['arg'],
            "body": ["primitive", ['literal-string', "number_plus"]],
            "is_ctor": False,
#            "owner": Number_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Number>.+ compiled function"}),
    _kernel_imodule)

Dictionary['dict']['+'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "+",
            "params": ['arg'],
            "body": ["primitive", ['literal-string', "dictionary_plus"]],
            "is_ctor": False,
#            "owner": Dictionary_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Dictionary>.+ compiled function"}),
    _kernel_imodule)


List['dict']['each'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "each",
            "params": ['fn'],
            "body": ["primitive", ['literal-string', "list_each"]],
            "is_ctor": False,
#            "owner": Dictionary_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<List>.each compiled function"}),
    _kernel_imodule)


MirrorBehavior['dict']['new'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "new",
            "params": ['mirrored'],
            "body": ["primitive", ['literal-string', "mirror_new"]],
            "is_ctor": True,
            "owner": Mirror['compiled_class'], #the CompiledClass for CompiledFunction class
            "@tag": "Mirror.new compiled function"}),
    _kernel_imodule)

Mirror['dict']['fields'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "fields",
            "params": [],
            "body": ["primitive", ['literal-string', "mirror_fields"]],
            "is_ctor": False,
#            "owner": Dictionary_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Mirror>.fields compiled function"}),
    _kernel_imodule)


Mirror['dict']['valueFor'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "valueFor",
            "params": ['name'],
            "body": ["primitive", ['literal-string', "mirror_value_for"]],
            "is_ctor": False,
#            "owner": Dictionary_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Mirror>.valueFor compiled function"}),
    _kernel_imodule)

Mirror['dict']['setValueFor'] = _function_from_cfunction(
    _create_compiled_function({
            "name": "valueFor",
            "params": ['name','value'],
            "body": ["primitive", ['literal-string', "mirror_set_value_for"]],
            "is_ctor": False,
#            "owner": Dictionary_CompiledClass, #the CompiledClass for CompiledFunction class
            "@tag": "<Mirror>.setValueFor compiled function"}),
    _kernel_imodule)

def _instantiate_module(compiled_module, args, parent_module):
    #creates the Module object and its instance
    size = len(compiled_module["params"])+\
        len(compiled_module["compiled_classes"])+\
        len(compiled_module["compiled_functions"])

    imod_dictionary = {}

    # Module
    module = _create_module({"_vt": ModuleBehavior,
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
            super_class = parent_module[super_name] # would be actually a msg send
        elif super_name in compiled_module["compiled_classes"]:
            super_later[c["name"]] = super_name
        else:
            raise Exception("super class not found:" + super_name)

        # superclass BarClass found
        if super_class:
            # FooClassBehavior
            cbehavior = {"_vt": Behavior,
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
            cbehavior = {"_vt": Behavior,
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

class FunctionLoader(): # for eval
    def load_body(self, code, params, owner, env):
        self.parser = MemeParser("{"+code+"}")
        try:
            ast,err = self.parser.apply("as_eval")
            print "--EVAL AST"
            print ast
            print "//--EVAL AST"
        except Exception as err:
            print(err.formatError(''.join(self.parser.input.data)))
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
                                                         'env_table': self.env_id_table[0],
                                                         'uses_env': uses_env,
                                                         'env_table_skel': dict(zip(range(0,self.env_idx),[None]*self.env_idx)),
                                                         'owner': owner,
                                                         "@tag":"a compiled literal function"}))

        loader = Loader([ast])
        loader.i = self
        loader.apply("load_body")
        return self.functions[0]

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

    def l_done_literal_function(self):
        function = self.functions.pop()

        body = function["body"]

        function['env_table'] = self.env_id_table.pop()

        #hack to identify the literal function when executing it
        #body[0]: the first instruction. The body changes id, unfortunately
        #literal is in the parent fun, so it can be easily fetched during execution
        self.functions[-1]["fun_literals"][id(body[0])] = function

    def l_var_def(self, name):
        self.env_id_table[-1][self.env_idx] = name
        self.env_idx = self.env_idx + 1


class ModuleLoader():
    def load_module(self, script):
        src = open(script).read()
        parser = MemeParser(src)
        try:
            ast,_ = parser.apply("start")
        except Exception as err:
            print(err.formatError(''.join(parser.input.data)))
            sys.exit(1)

        print "---- AST ----"
        print ast
        print "//---- AST ----"
        self.current_module = _create_compiled_module({"filepath": script,
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

    def l_end_function(self, body):
        function = self.functions.pop()
        function['body'] = body

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

    def l_done_literal_function(self):
        function = self.functions.pop()

        body = function["body"]

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

class Interpreter():
    def __init__(self):
        self.module_loader = ModuleLoader()
        self.fun_loader = FunctionLoader()

        self.compiled_modules = {}

        # registers
        self.r_mp  = None  # module pointer
        self.r_cp  = None  # context pointer
        self.r_rp  = None  # receiver pointer
        self.r_rdp = None  # receiver data pointer
        self.r_ep  = None  # environment pointer

    def instantiate_module(self, compiled_module, args, imodule):
        return _instantiate_module(compiled_module, args, imodule)

    def kernel_module_instance(self):
        return _kernel_imodule

    def alloc(self, klass, data):
        return _create_instance(klass, data)

    # puff...
    def get_class(self, name):
        return globals()[name]

    def create_compiled_function(self, data):
        return _create_compiled_function(data)

    def create_function_from_cfunction(self, cfun, imodule):
        return _function_from_cfunction(cfun, imodule)

    def compiled_function_to_context(self, cfun, env, imodule):
        return _compiled_function_to_context(cfun, env, imodule)

    def compile_code(self, text, params, owner, env):
        return self.fun_loader.load_body(text, params, owner, env)

    # object routines
    # -dealing with nulls, booleans, integers, etc...

    def get_vt(self, obj):
        if obj == None:
            return Object
        elif isinstance(obj, basestring):
            return String
        elif isinstance(obj, dict) and '_vt' not in obj:
            return Dictionary
        elif isinstance(obj, int) or isinstance(obj, long):
            return Number
        elif isinstance(obj, list):
            return List
        else:
            return obj["_vt"]

    def has_slot(self, obj, name):
        if obj == None:
            return False
        elif hasattr(obj, '__iter__'):
            return name in obj
        else:
            return False

    # execution

    def start(self, main_script):
        compiled_module = self.module_loader.load_module(main_script)
        imodule = _instantiate_module(compiled_module, {}, _kernel_imodule)
        self.run_module(imodule)

    def run_module(self, module):
        fun = module["main"]

        # registers
        self.r_mp  = module
        self.r_rp  = module # module.main()
        self.r_rdp = module
        self.stack = [{}]

        ret = self.setup_and_run_fun(module, module, module['main'], [], True)
        print "RETVAL: " + str(ret)

    def _lookup(self, drecv, vt, selector):
        if vt == None:
            return None, None

        if selector in vt["dict"]:
            return drecv, vt["dict"][selector]
        else:
            parent = vt["parent"]
            if self.has_slot(drecv, "_delegate"):
                delegate = drecv["_delegate"]
            else:
                delegate = None
            return self._lookup(delegate, parent, selector)

    # get the right rdp for ctors
    def ctor_rdp_for(self, rp, fun):
        if rp == None:
            raise Exception("No rdp for ctor. Probably a bug")

        klass = self.get_vt(rp)['compiled_class']
        if klass == fun['compiled_function']['owner']:
            return rp
        else:
            return self.ctor_rdp_for(rp['_delegate'], fun)
        #if rp['_vt'] == fun

    def run_fun(self, fun, should_allocate):
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

        self.evaluator = Eval([fun["compiled_function"]["body"]])
        self.evaluator.i = self
        try:
            ret,err = self.evaluator.apply("exec_fun")
        except ReturnException as e:
            ret = e.val
        self.tear_fun()
        return ret

    def tear_fun(self):
        frame = self.stack.pop()
        self.r_cp = frame["r_cp"]
        self.r_rp = frame["r_rp"]
        self.r_rdp = frame["r_rdp"]
        self.r_ep  = frame["r_ep"]
        if self.r_cp: #if we are exiting main, this will be null
            self.r_mp = self.r_cp["module"]

    def setup_and_run_fun(self, recv, drecv, method, args, should_allocate):
        if len(method["compiled_function"]["params"]) != len(args):
            raise Exception("arity error: " + method['compiled_function']['name'] + "::"+pformat(args,1,80,2))

        #backup frame
        self.stack.append({
                "r_cp": self.r_cp,
                "r_rp": self.r_rp,
                "r_rdp": self.r_rdp,
                "r_ep" : self.r_ep})
                # no need to backup mp, it can be infered from current fun

        # binding up arguments to parameters
        self.r_cp  = method
        self.r_mp = method["module"]
        self.r_ep = None
        if self.get_vt(method) != Context:
            self.r_rp  = recv
            self.r_rdp = drecv
            if not method["compiled_function"]["uses_env"] and self.get_vt(method) == Function:
                # normal fun, put args in the stack
                for k,v in zip(method["compiled_function"]["params"],args):
                    self.stack[-1][k] = v
            # normal fun using env, initialize one
            elif method["compiled_function"]["uses_env"] and self.get_vt(method) == Function:
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

        return self.run_fun(method, should_allocate)

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
        return globals()['prim_'+prim_name](self)

    def eval_do_field_attr(self, field, rhs):
        if not field in self.r_rdp:
            raise Exception("object has no field " + field)
        else:
            self.r_rdp[field] = rhs

    def eval_do_local_attr(self, name,expr):
        if self.r_ep != None:
            self.env_set_value(name, expr)
        elif name in self.stack[-1]:
            self.stack[-1][name] = expr
        else:
            raise Exception("Undeclared variable: " + name)

    def eval_do_var_def(self, name, expr):
        if self.r_ep != None:
            self.env_set_value(name, expr)
        else:
            self.stack[-1][name] = expr

    def eval_do_fun_lit(self, params, body):
        compiled_fun = self.r_cp["compiled_function"]["fun_literals"][id(body[0])]
        return _compiled_function_to_context(compiled_fun, self.r_ep, self.r_mp)

    def eval_access_field(self, field):
        if field in self.r_rdp:
            return self.r_rdp[field]
        else:
            raise Exception("object has no field " + field)

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
            if name in self.stack[-1]:
                return self.stack[-1][name]

        #-module params + module entries
        if name in self.r_mp:
            return self.r_mp[name]
        else:
            raise Exception("Undeclared var: " + name + pformat(self.r_cp['compiled_function'],1,80,1))

    def eval_do_return(self, value):
        raise ReturnException(value)

    def eval_do_super_ctor_send(self, selector, args):
        # -super.foo() can only be used inside constructors.
        # -normal methods can invoke super() to get its superclass version.
        # -A method m() cannot invoke a super method of different name.
        if not self.r_cp["compiled_function"]["is_ctor"]:
            raise Exception("Cannot use super.m() outside ctor");

        instance = self.r_rdp
        klass = self.get_vt(instance)
        pklass = klass["parent"]
        receiver = instance["_delegate"]

        drecv, method = self._lookup(receiver, self.get_vt(pklass), selector)

        if not method:
            raise Exception("DoesNotUnderstand: " + selector + " -- " + pformat(instance,1,80,1))
        elif not method["compiled_function"]["is_ctor"]:
            raise Exception("Method is not constructor: " + selector)
        else:
            return self.setup_and_run_fun(receiver, drecv, method, args, False)

    def eval_do_bin_send(self, selector, receiver, arg):
        drecv, method = self._lookup(receiver, self.get_vt(receiver), selector)
        if not method:
            raise Exception("DoesNotUnderstand: " + selector + " -- " + pformat(receiver,1,80,1))
        else:
            return self.setup_and_run_fun(receiver, drecv, method, [arg], True)


    def eval_do_send(self, receiver, selector, args):
        drecv, method = self._lookup(receiver, self.get_vt(receiver), selector)
        if not method:
            raise Exception("DoesNotUnderstand: " + selector  + " -- " + pformat(receiver,1,80,1))

        else:
            return self.setup_and_run_fun(receiver, drecv, method, args, True)

    def eval_do_call(self, fun, args):
        return self.setup_and_run_fun(self.r_rp, self.r_rdp, fun, args, True)


    def eval_do_send_or_call(self, name, args):
        fn = None
        # if name is local...
        if self.r_ep:
            idx = self.env_lookup(name) # in env?
            if idx != None:
                fn = self.r_ep[idx]
        elif name in self.stack[-1]:        # local in stack
            fn = self.stack[-1][name]

        if fn:
            return self.setup_and_run_fun(self.r_rp, self.r_rdp, fn, args, True)

        if name in self.r_mp:            # shared
            fn = self.r_mp[name]
            return self.setup_and_run_fun(self.r_mp, self.r_mp, fn, args, True)

        raise Exception("Undeclared function: " + name)

    def eval_do_if(self, cond, yes):
        if cond != False:
            self.evaluator.apply("exprlist", yes)

    def eval_do_if_else(self, cond, yes, no):
        if cond != False:
            self.evaluator.apply("exprlist", yes)
        else:
            self.evaluator.apply("exprlist", no)

if len(sys.argv) == 1:
    print "i.py <source.mm>"
    sys.exit(0)

Interpreter().start(sys.argv[1])
