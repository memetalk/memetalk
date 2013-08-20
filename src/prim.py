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

from PyQt4 import QtGui, QtCore, QtWebKit
from PyQt4.QtWebKit import *
from PyQt4.QtGui import *
from PyQt4.QtCore import *

import sys
import re
from os import listdir
from os.path import isfile, join
from pdb import set_trace as br
#import dbgui
import scintilla_editor
from config import MODULES_PATH
import traceback
from mmpprint import P

_app = None
_qapp_running = False
_qt_imodule = None

def prim_modules_path(proc):
    return MODULES_PATH

def prim_available_modules(proc):
    return [f[:-3] for f in listdir(MODULES_PATH) if isfile(join(MODULES_PATH,f)) and f != "core.md"]

def prim_get_module(proc):
    return proc.interpreter.compiled_module_by_filename(proc.locals['name'] + ".mm")

def prim_save_module(proc):
    return proc.interpreter.save_module(proc.locals['name'])

def prim_break_at(proc):
    cfun = proc.locals['cfun']
    line = proc.locals['line']
    proc.interpreter.break_at(cfun, line)
    return proc.r_rdp

def prim_import(proc):
    compiled_module = proc.interpreter.compiled_module_by_filename(proc.locals["mname"])
    args = dict(zip(compiled_module["params"],proc.locals["margs"]))
    imodule = proc.interpreter.instantiate_module(compiled_module, args, proc.interpreter.kernel_module_instance())
    return imodule

def prim_exception_raise(proc):
    proc.interpreter.throw(proc.r_rp)

def prim_vmprocess(proc):
    VMProcessClass = proc.interpreter.get_vt(proc)
    return proc.interpreter.alloc_object(VMProcessClass, {'self':proc})

def prim_vmprocess_stack_frames(proc):
    _proc = _lookup_field(proc, proc.r_rp, 'self')
    VMStackFrameClass = proc.interpreter.get_core_class('VMStackFrame')
    # first r_cp on stack is None
    return [proc.interpreter.alloc_object(VMStackFrameClass,{'self':x}) for x in _proc.stack if x['r_cp']] +\
        [proc.interpreter.alloc_object(VMStackFrameClass,{'self':_proc.top_frame()})]

def prim_vmprocess_step_into(proc):
    #print '+ENTER prim_vmprocess_step_into'
    _proc = _lookup_field(proc, proc.r_rp, 'self')
    #print 'vmprocess: sending step_into'
    ret = _proc.switch("step_into", None)
    #print 'vmprocess/step_into received: ' + P(ret,1,True)
    if 'done' in ret:
        #print 'done...exiting qevent'
        eventloop_processes[-1]['done'] = True
        return False
    #print 'vmprocess/step_over DONE'
    if 'exception' in ret:
        return ret[1]
    else:
        return True

def prim_vmprocess_step_over(proc):
    #print '+ENTER prim_vmprocess_step_over'
    _proc = _lookup_field(proc, proc.r_rp, 'self')
    #print 'vmprocess: sending step_over'
    ret = _proc.switch("step_over", None)
    #print 'vmprocess/step_over received: ' + P(ret,1,True)
    if 'done' in ret:
        #print 'done...exiting qevent'
        eventloop_processes[-1]['done'] = True
        return False
    #print 'vmprocess/step_over DONE'
    if 'exception' in ret:
        return ret[1]
    else:
        return True

# def prim_vmprocess_continue(proc):
#     print '+ENTER prim_vmprocess_continue'
#     _proc = _lookup_field(proc, proc.r_rp, 'self')
#     print 'vmprocess: sending continue'
#     ret = _proc.switch("continue")
#     print 'vmprocess/continue received: ' + P(ret,1, True)
#     if 'done' in ret:
#         print 'done...exiting qevent'
#         eventloop_processes[-1]['qtobj'].exit(0)
#         eventloop_processes[-1]['done'] = True
#         return False
#     print 'vmprocess/continue DONE'
#     return True

def prim_vmprocess_reload_frame(proc):
    _proc = _lookup_field(proc, proc.r_rp, 'self')
    _proc.switch("reload", proc.locals['line'])

def prim_vmprocess_rewind_until(proc):
    _proc = _lookup_field(proc, proc.r_rp, 'self')
    frame = _lookup_field(proc, proc.locals['fromFrame'], 'self')
    _proc.switch("rewind", frame, proc.locals['to_line'])

def prim_vmprocess_set_debugger_process(proc):
    _proc = _lookup_field(proc, proc.r_rp, 'self')
    #print 'set_dbg_proc: ' + str(proc.locals['arg'])
    _proc.debugger_process = proc.locals['arg']

def prim_vmprocess_debug(proc):
    fn = proc.locals['fn']
    args = proc.locals['args']
    proc.state = 'paused'
    return proc.setup_and_run_fun(None, None, fn['compiled_function']['name'], fn, args, True)

def prim_vmprocess_stop_on_exception(proc):
    proc.flag_stop_on_exception = True

# dirty stuff ahead: I'm too lazy to create a memetalk class wrapper over ASTNode
def prim_vmstackframe_instruction_pointer(proc):
    frame = _lookup_field(proc, proc.r_rp, 'self')
    ast = frame['r_ip']
    if ast == None: #first frame is none
        return None

    # ast has the line numbering relative to the entire module filre.
    # we need to make it relative to the toplevel function

    outer_cfun = proc.interpreter.shitty_get_module_from_cfunction(frame['r_cp']['compiled_function'])
    start_line = ast.start_line - outer_cfun['line']+1
    start_col = ast.start_col
    end_line = ast.end_line - outer_cfun['line']+1
    end_col = ast.end_col
    res = {"start_line":start_line, "start_col": start_col, "end_line": end_line, "end_col":end_col}
    return res

def prim_vmstackframe_module_pointer(proc):
    frame = _lookup_field(proc, proc.r_rp, 'self')
    if frame['r_cp'] != None: #first frame is none
        return frame['r_cp']['module']
    else:
        return None

def prim_vmstackframe_context_pointer(proc):
    frame = _lookup_field(proc, proc.r_rp, 'self')
    return frame['r_cp']

def prim_vmstackframe_receiver_pointer(proc):
    frame = _lookup_field(proc, proc.r_rp, 'self')
    return frame['r_rp']

def prim_vmstackframe_environment_pointer(proc):
    frame = _lookup_field(proc, proc.r_rp, 'self')
    return frame['r_ep']

def prim_vmstackframe_local_vars(proc):
    frame = _lookup_field(proc, proc.r_rp, 'self')
    return frame['locals']

def prim_io_print(proc):
    arg = proc.locals['arg']
    if isinstance(arg, basestring):
        print arg
    elif isinstance(arg, dict) and '_vt' not in arg:
        print arg
    else:
        P(proc.locals["arg"],4)

def prim_ast_line(proc):
    br()

def prim_object_to_string(proc):
    obj = proc.r_rp
    if obj == None:
        return "null"
    elif isinstance(obj, dict) and '_vt' in obj:
        return P(obj,1, True) #dirt and limited, str does inifinite loop
    else:
        return str(obj)

def prim_object_to_source(proc):
    obj = proc.r_rp
    if obj == None:
        return 'null'
    elif isinstance(obj, dict) and '@tag' in obj:
        return  '<' + proc.r_rp['@tag'] + '>'
    else:
        return P(obj,1,True)

def prim_object_equal(proc):
    return proc.r_rp == proc.locals['other']

def prim_object_not_equal(proc):
    return proc.r_rp != proc.locals['other']

# def prim_string_replace(i):
#     return re.sub(i.locals['what'],i.locals['for'],i.r_rp)

def prim_string_size(proc):
    return len(proc.r_rp)

def prim_string_concat(proc):
    return proc.r_rp + proc.locals['arg']

def prim_string_from(proc):
    return proc.r_rp[proc.locals['idx']:]

def prim_string_rindex(proc):
    try:
        return proc.r_rdp.rindex(proc.locals['arg'])
    except ValueError:
        return -1

def prim_symbol_to_string(proc):
    return proc.r_rdp['self']

def prim_string_count(proc):
    return proc.r_rp.count(proc.locals['sub'])

def prim_string_substring(proc):
    return proc.r_rp[proc.locals['from']:proc.locals['from']+proc.locals['count']]

def prim_string_split(proc):
    return proc.r_rp.split(proc.locals['sep'])

def prim_module_instance_compiled_module(proc):
    return proc.r_rdp['_vt']['compiled_module']

def prim_compiled_function_new_top_level(proc):
    name = proc.locals['name']
    text = proc.locals['text']
    owner = proc.locals['owner']
    flag = proc.locals['flag']
    cfun = proc.interpreter.compile_top_level(name,text,owner, flag)
    cfun['is_top_level'] = True
    return cfun

def prim_compiled_function_new_closure(proc):
    text = proc.locals['text']
    outer = proc.locals['cfun']
    cfun = proc.interpreter.compile_closure(text,outer)
    cfun['is_embedded'] = proc.locals['is_embedded'] #text is embeded in outter cfun?
    cfun['is_top_level'] = False
    return cfun

def prim_compiled_function_set_code(proc):
    if proc.r_rdp['is_top_level']:
        return proc.interpreter.recompile_top_level(proc.r_rdp, proc.locals['code'])
    else:
        # NOTE: if a Function of this cfun already exists, its env should
        # be updated, otherwise calling it will screw up var access.
        return proc.interpreter.recompile_closure(proc.r_rdp, proc.locals['code'])


# hackutility for as_context..
class ProxyEnv():
    def __init__(self, frame, cp_env_table):
        self.frame = frame
        self.table = dict(cp_env_table)

        self.i = len(self.table)
        for name in frame['locals'].keys():
            self.table[self.i] = name
            self.i = self.i + 1

    def __getitem__(self, key):
        if isinstance(key, (int, long)):
            return self.frame['locals'][self.table[key]]
        else:
            #r_rdp,r_rp
            return self.frame[key]

    def __setitem__(self, key, val):
        if isinstance(key, (int, long)):
            self.frame['locals'][self.table[key]] = val
        else:
            #r_rdp,r_rp
            self.frame[key] = val
    def __contains__(self, item):
        return item in ['r_rdp', 'r_rp'] + range(0,self.i)

    def __iter__(self):
        ret = {}
        for key, name in self.table.iteritems():
            ret[key] = self.frame['locals'][name]
        return ret.iteritems()

def prim_compiled_function_as_context_with_frame(proc):
    frame = proc.locals['frame']
    # If the frame passed has an r_ep, just use it as our env.
    # else, we need to construct a proxy env capable of changing
    # frame.locals. We also need to patch r_rp function's env_table
    # so that all frame.locals/this are mapped there (so env_lookup works).
    if frame['self']['r_ep'] != None:
        env = frame['self']['r_ep']
        # Though this cfun was already constructed with an outer_cfun,
        # now we have a frame, so it's cp should be our outer_cfun! :(
        proc.r_rp['outer_cfun'] = frame['self']['r_cp']['compiled_function']
        # patching our env_table's indexes
        begin = max(proc.r_rp['outer_cfun']['env_table'].keys()) + 1
        proc.r_rp['env_table'] = dict([(x+begin,y) for x,y in proc.r_rp['env_table'].iteritems()])
    else:
        env = ProxyEnv(frame['self'], proc.r_rp['env_table'])
        proc.r_rp['env_table'] = env.table # patching the env_table
    ret = proc.interpreter.compiled_function_to_context(proc.r_rp, env, proc.locals['imodule'])
    return ret

def prim_compiled_function_as_context_with_vars(proc):
    var_dict = proc.locals['vars']
    env = dict(var_dict) # we do 'del' below, lets no fuck with the parameter
    if 'this' in env: #another hack for binding this
        this = env['this']
        del env['this']
    else:
        this = None
    begin = len(proc.r_rp['env_table'])
    proc.r_rp['env_table'].update(dict(zip(range(begin,begin+len(env.keys())), env.keys())))
    env = dict([(key,env[name]) for key,name in proc.r_rp['env_table'].iteritems() if name in env])
    env['r_rdp'] = env['r_rp'] = this
    ret = proc.interpreter.compiled_function_to_context(proc.r_rp, env, proc.locals['imodule'])
    return ret

def prim_compiled_function_instantiate(proc):
    return proc.interpreter.create_function_from_cfunction(proc.r_rp, proc.locals['imodule'])

def prim_context_apply(proc):
    # fn.apply([...args...])
    args = proc.locals['args']
    return proc.setup_and_run_fun(None, None, proc.r_rdp['compiled_function']['name'], proc.r_rdp, args, True)

def prim_context_get_env(proc):
    #warning 1: no checking if the env idexes are ordered. we assume so.
    #warning 2: only the outer env is returned (closures declaring variables
    #                                            are not contemplated. its
    #                                            a TODO).
    env = dict(proc.r_rdp['env'])
    env_table = dict(proc.r_rdp['compiled_function']['env_table'])
    ret = {}
    for k,v in env_table.items():
        if k in env:
            ret[v] = env[k]
    return ret

def prim_function_apply(proc):
    # fn.apply([...args...])
    args = proc.locals['args']
    return proc.setup_and_run_fun(None, None, proc.r_rdp['compiled_function']['name'], proc.r_rdp, args, True)

def prim_number_plus(proc):
    return proc.r_rp + proc.locals['arg']

def prim_number_minus(proc):
    return proc.r_rp - proc.locals['arg']

def prim_number_lst(proc):
    return proc.r_rp < proc.locals['arg']

def prim_number_lsteq(proc):
    return proc.r_rp <= proc.locals['arg']

def prim_number_grteq(proc):
    return proc.r_rp >= proc.locals['arg']

def prim_dictionary_plus(proc):
    return dict(proc.r_rp.items() + proc.locals['arg'].items())

def prim_dictionary_each(proc):
    for key,val in proc.r_rdp.iteritems():
        proc.setup_and_run_fun(None, None, 'fn', proc.locals['fn'], [key,val], True)

def prim_dictionary_has(proc):
    return proc.locals['key'] in proc.r_rdp

def prim_dictionary_set(proc):
    proc.r_rdp['key'] = proc.locals["value"]

def prim_dictionary_remove(proc):
    del proc.r_rdp[proc.locals['key']]

def prim_dictionary_map(proc):
    ret = []
    for key,val in proc.r_rdp.iteritems():
        ret.append(proc.setup_and_run_fun(None, None, 'fn', proc.locals['fn'], [key,val], True))
    return ret

def prim_get_compiled_module(proc):
    return proc.interpreter.get_vt(proc.locals['module'])['compiled_module']

def prim_get_compiled_class(proc):
    return proc.locals['klass']['compiled_class']

def prim_get_current_process(proc):
    VMProcess = proc.interpreter.get_core_class('VMProcess')
    return proc.interpreter.alloc_object(VMProcess, {'self': proc})

def prim_compiled_class_constructors(proc):
    return dict([(name, cfun) for name,cfun in proc.r_rdp['own_methods'].iteritems() if cfun['is_ctor']])

def prim_compiled_class_rename(proc):
    klass = proc.r_rdp
    new_name = proc.locals['name']
    old_name = klass['name']
    cmod = klass['module']

    klass['name'] = new_name

    del cmod['compiled_classes'][old_name]
    cmod['compiled_classes'][new_name] = klass
    for imod in proc.interpreter.imodules():
        if imod['_vt']['compiled_module'] == cmod:
            iklass = imod[old_name]
            del imod[old_name]
            imod[new_name] = iklass
            del imod['_vt']['dict'][old_name] # del accessor
            imod['_vt']['dict'][new_name] = proc.interpreter.create_accessor_method(imod, new_name)
    return proc.r_rp

def prim_compiled_class_set_fields(proc):
    klass = proc.r_rdp
    fields = proc.locals['fields']

    def diff(a, b):
        b = set(b)
        return [aa for aa in a if aa not in b]

    rm =  diff(klass['fields'], fields)
    add = diff(fields, klass['fields'])

    for obj in proc.interpreter.memory:
        if 'compiled_class' in obj['_vt'] and obj['_vt']['compiled_class'] == klass:
            for f in rm: del obj[f]
            for f in add: obj[f] = None
    klass['fields'] = fields


def prim_compiled_class_class_methods(proc):
    return dict([(name, cfun) for name,cfun in proc.r_rdp['own_methods'].iteritems() if not cfun['is_ctor']])

def prim_compiled_class_add_method(proc):
    cfun = proc.locals['cfun']
    flag = proc.locals['flag']

    # add the function to the compiled class
    if flag['self'] == 'instance_method':
        proc.r_rdp['methods'][cfun['name']] = cfun
    elif flag['self'] == 'class_method' or flag['self'] == 'constructor':
        proc.r_rdp['own_methods'][cfun['name']] = cfun
    else:
        proc.interpreter.throw_with_value("Unknown flag: " + P(flag,1,True))

    # add the function to the instantiated classes:
    for imod in proc.interpreter.imodules():
        if imod['_vt']['compiled_module'] == proc.r_rdp['module']:
            fun = proc.interpreter.create_function_from_cfunction(cfun, imod)
            if flag['self'] == 'instance_method':
                imod[proc.r_rdp['name']]['dict'][cfun['name']] = fun
            else:
                imod[proc.r_rdp['name']]['_vt']['dict'][cfun['name']] = fun
    return proc.r_rp

def prim_compiled_class_remove_method(proc):
    name = proc.locals['name']
    flag = proc.locals['flag']
    cclass = proc.r_rdp

    # removing function from compiled class
    if flag['self'] == 'instance_method':
        del proc.r_rdp['methods'][name]
    else:
        del proc.r_rdp['own_methods'][name]

    # removing function from instances:
    for imod in proc.interpreter.imodules():
        if imod['_vt']['compiled_module'] == proc.r_rdp['module']:
            if flag['self'] == 'instance_method':
                del imod[proc.r_rdp['name']]['dict'][name]
            else:
                del imod[proc.r_rdp['name']]['_vt']['dict'][name]

def prim_compiled_class_method_flag(proc):
    if proc.locals['cfun'] in proc.r_rdp['methods'].values():
        return proc.interpreter.interned_symbol_for("instance_method")
    elif proc.locals['cfun'] in proc.r_rdp['own_methods'].values():
        return proc.interpreter.interned_symbol_for("class_method")
    return None

def prim_mirror_fields(proc):
    mirrored = proc.r_rdp['mirrored']
    if hasattr(mirrored, 'keys'):
        return mirrored.keys()
    elif isinstance(mirrored, list):
        return [str(x) for x in range(0,len(mirrored))]
    else:
        return []

def prim_mirror_value_for(proc):
    if isinstance(proc.r_rdp['mirrored'], list):
        return proc.r_rdp['mirrored'][int(proc.locals['name'])]
    else:
        return proc.r_rdp['mirrored'][proc.locals['name']]

def prim_mirror_set_value_for(proc):
    if isinstance(proc.r_rdp['mirrored'], list):
        proc.r_rdp['mirrored'][int(proc.locals['name'])] = proc.locals['value']
    else:
        proc.r_rdp['mirrored'][proc.locals['name']] = proc.locals['value']
    return proc.r_rp

def prim_mirror_vt(proc):
    return proc.interpreter.get_vt(proc.locals['obj'])

def prim_list_each(proc):
    for x in proc.r_rdp:
        proc.setup_and_run_fun(None, None, 'fn', proc.locals['fn'], [x], True)

def prim_list_get(proc):
    return proc.r_rdp[proc.locals['n']]

def prim_list_size(proc):
#    print("********** SIZE:"+str(len(proc.r_rdp)))
    return len(proc.r_rdp)

def prim_list_map(proc):
    return [proc.setup_and_run_fun(None, None, 'fn', proc.locals['fn'], [x], True) for x in proc.r_rdp]

def prim_list_plus(proc):
    return list(proc.r_rdp + proc.locals['arg'])

def prim_list_has(proc):
    return proc.locals['value'] in proc.r_rdp

def prim_list_add(proc):
    return proc.r_rdp.append(proc.locals['value'])

def prim_list_join(proc):
    return proc.locals['sep'].join(proc.r_rdp)

def prim_list_to_string(proc):
    ret = []
    for x in proc.r_rdp:
        if hasattr(x,'__str__'):
            ret.append(str(x))
        else:
            ret.append(P(x,1,True))
    return ', '.join(ret)

def prim_io_file_contents(proc):
    return open(proc.locals['path']).read()

def prim_compiled_module_remove_function(proc):
    name = proc.locals['name']
    cmod = proc.r_rdp

    # removing function from compiled module
    del cmod['compiled_functions'][name]

    # removing function from instances:
    for imod in proc.interpreter.imodules():
        if imod['_vt']['compiled_module'] == cmod:
            # remove the getter...
            del imod['_vt']['dict'][name]
            # ...and the python dict field
            del imod[name]

def prim_compiled_module_add_function(proc):
    cfun = proc.locals['cfun']
    cmod = proc.r_rdp

    # add the function to the compiled module
    cmod['compiled_functions'][cfun['name']] = cfun

    # add the function to the module instances:
    for imod in proc.interpreter.imodules():
        if imod['_vt']['compiled_module'] == cmod:
            fun = proc.interpreter.create_function_from_cfunction(cfun, imod)
            # add the getter
            imod['_vt']['dict'][cfun['name']] = fun
            # add the function to the dict:
            imod[cfun['name']] = fun

def prim_compiled_module_new_class(proc):
    name = proc.locals['name']
    super_name = proc.locals['super_name']
    cmod = proc.r_rdp
    klass = proc.interpreter.create_compiled_class({"name": name,
                                                    "super_class_name":super_name,
                                                    "module": cmod})

    cmod['compiled_classes'][name] = klass

    for imod in proc.interpreter.imodules():
        if imod['_vt']['compiled_module'] == cmod:
            cb = {"_vt": proc.interpreter.get_core_class("Behavior"),
                  "parent": proc.interpreter.get_core_class("ObjectBehavior")['_vt'],
                  "dict": {},
                  "@tag": name + "Behavior"}
            imod['_vt']['dict'][name] = proc.interpreter.create_accessor_method(imod, name)
            imod[name] = proc.interpreter.create_class({"_vt": cb,
                                                        "parent": proc.interpreter.get_core_class("Object"),
                                                        "dict": {},
                                                        "compiled_class": klass,
                                                        "@tag": name + " Class"})
    return klass

def prim_compiled_module_add_class(proc):
    klass = proc.locals['klass']
    name = klass['name']
    cmod = proc.r_rdp
    klass['module'] = cmod

    cmod['compiled_classes'][name] = klass
    for imod in proc.interpreter.imodules():
        if imod['_vt']['compiled_module'] == cmod:
            cb = {"_vt": proc.interpreter.get_core_class("Behavior"),
                  "parent": proc.lookup_in_modules(klass["super_class_name"], imod)['_vt'],
                  "dict": proc.interpreter.compiled_functions_to_functions(klass["own_methods"], imod),
                  "@tag": name + "Behavior"}
            imod['_vt']['dict'][name] = proc.interpreter.create_accessor_method(imod, name)
            imod[name] = proc.interpreter.create_class({"_vt": cb,
                                                        "parent": proc.interpreter.get_core_class("Object"),
                                                        "dict": {},
                                                        "compiled_class": klass,
                                                        "@tag": name + " Class"})
    return klass


def prim_compiled_module_remove_class(proc):
    name = proc.locals['name']
    cmod = proc.r_rdp
    del cmod['compiled_classes'][name]

    # removing function from instances:
    for imod in proc.interpreter.imodules():
        if imod['_vt']['compiled_module'] == cmod:
            del imod[name]
            del imod['_vt']['dict'][name]
    return proc.r_rp

def prim_compiled_module_default_parameter_for(proc):
    # really simplorious, for the moment
    if proc.locals['name'] in proc.r_rdp['default_params']:
        return proc.r_rdp['default_params'][proc.locals['name']]['value']
    else:
        return None

def prim_compiled_module_set_default_parameter(proc):
    # really simplorious, for the moment
    proc.r_rdp['default_params'][proc.locals['name']] = \
        {'name': proc.locals['name'], 'type':'lib','value':proc.locals['m']}
    return proc.r_rp

def prim_compiled_module_instantiate(proc):
    core = proc.interpreter.get_core_module()
    return proc.interpreter.instantiate_module(proc.r_rdp, proc.locals['args'], core)

# lookup the inner handle of the actual binding object
# which is set -- this is because a hierarchy of delegates
# will have the same field (say, QMainWindow < QWidget both
# have 'self', but QMainWindow.new() sets 'self' only to
# its leaf object, not to the delegated QWidget
def _lookup_field(proc, mobj, name):
    if mobj == None:
        proc.interpreter.throw_with_value('field not found: ' + name)
    if name in mobj and mobj[name] != None:
        return mobj[name]
    else:
        return _lookup_field(proc, mobj['_delegate'], name)

# def _set_field(mobj, name, value):
#     if mobj == None:
#         raise Exception('field not found: ' + name)
#     if name in mobj:
#         mobj[name] = value
#     else:
#         _set_field(mobj['_delegate'], name, value)
### Qt

## return a meme instance given something from pyqt
def _meme_instance(proc, obj):
    global _qt_imodule
    mapping = {
        QtGui.QListWidgetItem: _qt_imodule["QListWidgetItem"],
        QtWebKit.QWebView: _qt_imodule['QWebView'],
        QtWebKit.QWebFrame: _qt_imodule['QWebFrame'],
        QtWebKit.QWebPage: _qt_imodule['QWebPage'],
        QtWebKit.QWebElement: _qt_imodule['QWebElement'],
        QtGui.QAction: _qt_imodule['QAction'],
        QtCore.QUrl: _qt_imodule['QUrl'],
        QtGui.QMenuBar: _qt_imodule['QMenuBar'],
        scintilla_editor.MemeQsciScintilla: _qt_imodule['QsciScintilla']}

    if obj == None:
        return obj
    elif isinstance(obj, basestring):
        return obj# {"_vt":i.get_core_class("String"), 'self':obj}
    elif isinstance(obj, dict) and '_vt' not in obj:
        return obj#{"_vt":i.get_core_class("Dictionary"), 'self':obj}
    elif isinstance(obj, int) or isinstance(obj, long):
        return obj#{"_vt":i.get_core_class("Number"), 'self':obj}
    elif isinstance(obj, list):
        return obj#{"_vt":i.get_core_class("List"), 'self':obj}
    # elif isinstance(obj, QtCore.QUrl):
    #     return qstring_to_str(obj.toString()) # TODO: currently ignoring QUrl objects
    elif obj.__class__ in mapping: # NOTE: should be a qt instance
        if hasattr(obj, 'meme_instance'): # all qt objetcs should have this one day
            return obj.meme_instance
        else:
            return {"_vt":mapping[obj.__class__], 'self':obj}
    else:
        print "*** WARNING: object has no memetalk mapping specified:"
        P(obj)
        return None

def qstring_to_str(qstring):
    #return str(qstring.toUtf8()).decode("utf-8")
    return unicode(qstring.toUtf8(), "utf-8")

eventloop_processes = []

def prim_qt_qapplication_new(proc):
    global _app, _qt_imodule
    _app = QtGui.QApplication(sys.argv)
    _qt_imodule = proc.r_mp
    proc.r_rdp['self'] = _app
    return proc.r_rp

def prim_qt_qapplication_exec(proc):
    global _qapp_running
    _qapp_running = True
    eventloop_processes.append({'proc':proc, 'qtobj':proc.r_rdp['self'], 'done': False})
    return proc.r_rdp['self'].exec_()

def prim_qt_qapplication_focus_widget(proc):
    w = QApplication.focusWidget()
    return _meme_instance(proc,w)


def prim_qt_qeventloop_new(proc):
    qev = QtCore.QEventLoop()
    proc.r_rdp['self'] = qev
    return proc.r_rp

def prim_qt_qeventloop_exec(proc):
    eventloop_processes.append({'proc':proc, 'qtobj':proc.r_rdp['self'], 'done': False})
    return proc.r_rdp['self'].exec_()

def prim_qt_qeventloop_exit(proc):
    #print 'done...exiting qevent'
    eventloop_processes[-1]['qtobj'].exit(proc.locals['code'])
    eventloop_processes[-1]['done'] = True
    return proc.r_rdp['self'].exit(proc.locals['code'])

def prim_qt_qapplication_exit(proc):
    #print 'done...exiting qevent'
    eventloop_processes[-1]['qtobj'].exit(proc.locals['code'])
    eventloop_processes[-1]['done'] = True
    return proc.r_rdp['self'].exit(proc.locals['code'])

# QWidget
def prim_qt_qwidget_new(proc):
    # new(QWidget parent)
    parent = proc.locals['parent']
    if parent != None:
        qt_parent = _lookup_field(proc, parent, 'self')
        proc.r_rdp["self"] = QtGui.QWidget(qt_parent)
    else:
        proc.r_rdp['self'] = QtGui.QWidget()
    return proc.r_rp

def prim_qt_qwidget_set_focus(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setFocus()
    return proc.r_rp

def prim_qt_qwidget_set_maximum_height(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setMaximumHeight(proc.locals['h'])
    return proc.r_rp

def prim_qt_qwidget_set_minimum_size(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setMinimumSize(proc.locals['w'],proc.locals['h'])
    return proc.r_rp

def prim_qt_qwidget_set_minimum_width(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setMinimumWidth(proc.locals['w'])
    return proc.r_rp


def prim_qt_qwidget_show(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.show()
    return proc.r_rp

def prim_qt_qwidget_hide(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.hide()
    return proc.r_rp

def prim_qt_qwidget_set_window_title(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setWindowTitle(proc.locals['title'])
    return proc.r_rp

def prim_qt_qwidget_resize(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    w = proc.locals["w"]
    h = proc.locals["h"]
    qtobj.resize(w,h)
    return proc.r_rp

def prim_qt_qwidget_add_action(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qt_action = _lookup_field(proc, proc.locals["action"], 'self')
    qtobj.addAction(qt_action)
    return proc.r_rp

def prim_qt_qwidget_set_maximum_width(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setMaximumWidth(proc.locals['w'])
    return proc.r_rp

def prim_qt_qwidget_connect(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    signal = proc.locals['signal']
    slot = proc.locals['slot']
    def callback(*rest):
        args = []
        for arg in rest:
            args.append(_meme_instance(proc,arg))
        try:
            proc.setup_and_run_fun(None, None, '<?>', slot, args, True)
        except proc.interpreter.py_memetalk_exception() as e:
            print "Exception raised: " + e.mmobj()['value']
            print traceback.format_exc()

    getattr(qtobj,signal).connect(callback)
    return proc.r_rp

def prim_qt_qwidget_actions(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    r = [_meme_instance(proc, x) for x in qtobj.actions()]
    return r

def prim_qt_qwidget_set_stylesheet(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setStyleSheet(proc.locals['s'])
    return proc.r_rp

def prim_qt_qwidget_is_visible(proc):
    return _lookup_field(proc, proc.r_rp, 'self').isVisible()

def prim_qt_qwidget_close(proc):
    return _lookup_field(proc, proc.r_rp, 'self').close()

def prim_qt_qwidget_has_focus(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qtobj.hasFocus()

# QMainWindow

class _QMainWindow(QtGui.QMainWindow):
    def __init__(self, proc, meme_obj, parent=None):
        super(QMainWindow, self).__init__(parent)
        self.meme_obj = meme_obj
        self.proc = proc

    def closeEvent(self, ev):
        if 'closeEvent' in self.meme_obj['_vt']['dict']:
            fn = self.meme_obj['_vt']['dict']['closeEvent']
            self.proc.setup_and_run_fun(self.meme_obj, self.meme_obj, 'closeEvent', fn, [], True)

def prim_qt_qmainwindow_new(proc):
    proc.r_rdp['self'] = _QMainWindow(proc, proc.r_rp)
    return proc.r_rp

def prim_qt_qmainwindow_set_central_widget(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setCentralWidget(_lookup_field(proc, proc.locals['widget'],'self'))
    return proc.r_rp

# Warning! QMenuBar inherits QWidget!
# however we did not create the QWidget instance delegate
# here.
def prim_qt_qmainwindow_menu_bar(proc):
    QMenuBarClass = proc.r_mp["QMenuBar"]
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qt_bar_instance = qtobj.menuBar()
    return proc.interpreter.alloc_object(QMenuBarClass, {'self':qt_bar_instance})

def prim_qt_qmainwindow_status_bar(proc):
    QWidgetClass = proc.r_mp["QWidget"]
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qt_bar_instance = qtobj.statusBar()
    return proc.interpreter.alloc_object(QWidgetClass, {'self':qt_bar_instance})

# QPlainTextEdit
def prim_qt_qplaintextedit_new(proc):
    # new(QWidget parent)
    parent = proc.locals['parent']
    if parent != None:
        qt_parent = _lookup_field(proc, parent, 'self')
    else:
        qt_parent = None
    proc.r_rdp["self"] = QtGui.QPlainTextEdit(qt_parent)
    return proc.r_rp

def prim_qt_qplaintextedit_set_tabstop_width(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setTabStopWidth(proc.locals["val"])
    return proc.r_rp

def prim_qt_qplaintextedit_text_cursor(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qt_cursor = qtobj.textCursor()
    QTextCursorClass = proc.r_mp["QTextCursor"]
    return proc.interpreter.alloc_object(QTextCursorClass, {'self':qt_cursor})

def prim_qt_qplaintextedit_set_text_cursor(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    cursor = _lookup_field(proc, proc.locals['cursor'], 'self')
    qtobj.setTextCursor(cursor)
    return proc.r_rp

def prim_qt_qplaintextedit_set_plain_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setPlainText(proc.locals['text'])
    return proc.r_rp


def prim_qt_qplaintextedit_to_plain_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.toPlainText())



# QMenuBar
# Warning! QMenuBar inherits QWidget!
# however we did not create the QWidget instance delegate
# here.
def prim_qt_qmenubar_add_menu(proc):
    label = proc.locals["str"]
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qt_menu_instance = qtobj.addMenu(label)
    QMenuClass = proc.r_mp["QMenu"]
    return proc.interpreter.alloc_object(QMenuClass, {'self':qt_menu_instance})

# QAction
def prim_qt_qaction_new(proc):
    label = proc.locals['label']
    parent = proc.locals['parent']
    if parent != None:
        qt_parent = _lookup_field(proc, parent, 'self')
    else:
        qt_parent = None
    proc.r_rdp["self"] = QtGui.QAction(label,qt_parent)
    return proc.r_rp

def prim_qt_qaction_connect(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    signal = proc.locals["signal"]
    slot = proc.locals["slot"]
    def callback(*rest):
        try:
            proc.setup_and_run_fun(None, None, '<?>', slot, [], True)
        except proc.interpreter.py_memetalk_exception() as e:
            print "Exception raised: " + e.mmobj()['value']
            print traceback.format_exc()
        #print 'callback slot: ' + str(slot['compiled_function']['body'])
        #print 'callback: eventloop proc equal? ' + str(proc == eventloop_processes[-1]['proc'])
        if proc != eventloop_processes[-1]['proc']:
            entry = eventloop_processes[-1]
            #entry['qtobj'].exit(0)
            entry['proc'].switch('done', None) # this is were the exit point of the debugger arrives
            #print 'debugger module ended'
            proc.interpreter.debugger_process = None
            proc.interpreter.processes.remove(entry['proc'])
        if eventloop_processes[-1]['done']:
            #print 'POPing eventloop'
            eventloop_processes.pop()
        #print "END of callback"

    getattr(qtobj,signal).connect(callback)
    return proc.r_rp

def prim_qt_qaction_set_shortcut(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setShortcut(proc.locals["shortcut"]);
    return proc.r_rp

def prim_qt_qaction_set_shortcut_context(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setShortcutContext(proc.locals["context"]);
    return proc.r_rp

def prim_qt_qaction_set_enabled(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setEnabled(proc.locals["val"]);
    return proc.r_rp

def prim_qt_qshortcut_set_context(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setContext(proc.locals["context"]);
    return proc.r_rp

def prim_qt_qshortcut_new(proc):
    keys = proc.locals['sc']
    parent = proc.locals['parent']
    slot = proc.locals['slot']

    if parent != None:
        qt_parent = _lookup_field(proc, parent, 'self')
    else:
        qt_parent = None

    proc.r_rdp["self"] = QtGui.QShortcut(keys,qt_parent)
    proc.r_rdp["self"].setKey(keys)

    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    def callback(*rest):
        proc.setup_and_run_fun(None, None, '<?>', slot, [], True)
        #print 'callback slot: ' + str(slot['compiled_function']['body'])
        #print 'callback: eventloop proc equal? ' + str(proc == eventloop_processes[-1]['proc'])
        if proc != eventloop_processes[-1]['proc']:
            entry = eventloop_processes[-1]
            #entry['qtobj'].exit(0)
            entry['proc'].switch('done', None) # this is were the exit point of the debugger arrives
            #print 'debugger module ended'
            proc.interpreter.debugger_process = None
            proc.interpreter.processes.remove(entry['proc'])
            proc.state = 'running'
        if eventloop_processes[-1]['done']:
            #print 'POPing eventloop'
            eventloop_processes.pop()
        #print "END of callback"

    qtobj.activated.connect(callback)
    return proc.r_rp

#     def callback():
#         proc.setup_and_run_fun(None, None, '<?>', fn, [], True)

#     shortcut = QtGui.QShortcut(QtGui.QKeySequence(keys), qtobj, callback)
#     shortcut.setContext(QtCore.Qt.WidgetShortcut)
#     return proc.r_rp

# QTextCursor

def prim_qt_qtextcursor_selected_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')

    # Qt docs say \u2029 is a paragraph, and should be replaced with \n
    # TODO: make this replace works written in memetalk instead of hardcoded
    # here.
    return qstring_to_str(qtobj.selectedText()).replace(u'\u2029', '\n')

def prim_qt_qtextcursor_selection_end(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qtobj.selectionEnd()

def prim_qt_qtextcursor_set_position(proc):
    pos = proc.locals['pos']
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setPosition(pos)
    return proc.r_rp

def prim_qt_qtextcursor_insert_text(proc):
    text = proc.locals['text']
    string = text.decode('string_escape')
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.insertText(string)
    return proc.r_rp

def prim_qt_qtextcursor_drag_right(proc):
    length = proc.locals['len']
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    res = qtobj.movePosition(
        QtGui.QTextCursor.Left, QtGui.QTextCursor.KeepAnchor, length)
    return True if res else False

#QLayout

def prim_qt_qlayout_add_widget(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    parent = _lookup_field(proc, proc.locals['widget'], 'self')
    qtobj.addWidget(parent)
    return proc.r_rp

# QVBoxLayout
def prim_qt_qvboxlayout_new(proc):
    parent = proc.locals['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = QtGui.QVBoxLayout(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QVBoxLayout()
    return proc.r_rp

def prim_qt_qvboxlayout_add_layout(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    parent = _lookup_field(proc, proc.locals['layout'], 'self')
    qtobj.addLayout(parent)
    return proc.r_rp

def prim_qt_qvboxlayout_add_widget(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    w = _lookup_field(proc, proc.locals['widget'], 'self')
    qtobj.addWidget(w)
    return proc.r_rp



# QHBoxLayout
def prim_qt_qhboxlayout_new(proc):
    parent = proc.locals['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = QtGui.QHBoxLayout(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QHBoxLayout()
    return proc.r_rp

def prim_qt_qhboxlayout_add_widget(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    w = _lookup_field(proc, proc.locals['widget'], 'self')
    qtobj.addWidget(w)
    return proc.r_rp

def prim_qt_qhboxlayout_set_contents_margins(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setContentsMargins(proc.locals['l'],proc.locals['t'],proc.locals['r'],proc.locals['b'])
    return proc.r_rp

# QListWidget
def prim_qt_qlistwidget_new(proc):
    parent = proc.locals['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = QtGui.QListWidget(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QListWidget()
    return proc.r_rp

def prim_qt_qlistwidget_current_item(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return _meme_instance(proc,qtobj.currentItem())

# QListWidgetItem
def prim_qt_qlistwidgetitem_new(proc):
    txt = proc.locals['text']
    parent = proc.locals['parent']
    qtparent = None if parent == None else _lookup_field(proc, parent, 'self')
    proc.r_rdp['self'] = QtGui.QListWidgetItem(txt,qtparent)
    return proc.r_rp

def prim_qt_qlistwidgetitem_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.text())


#QLineEdit
def prim_qt_qlineedit_new(proc):
    parent = proc.locals['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = QtGui.QLineEdit(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QLineEdit()
    return proc.r_rp

def prim_qt_qlineedit_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.text())

def prim_qt_qlineedit_set_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setText(proc.locals['text'])
    return proc.r_rp

def prim_qt_qlineedit_selected_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.selectedText())

def prim_qt_qlineedit_select_all(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.selectAll()
    return proc.r_rp

def prim_qt_qlineedit_set_selection(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setSelection(proc.locals['start'], proc.locals['length'])
    return proc.r_rp


def prim_qt_qlabel_new(proc):
    parent = proc.locals['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = QtGui.QLabel(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QLabel()
    return proc.r_rp

def prim_qt_qlabel_set_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setText(proc.locals['text'])
    return proc.r_rp

def prim_qt_qheaderview_hide(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.hide()
    return proc.r_rp

def prim_qt_qheaderview_set_stretch_last_section(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setStretchLastSection(proc.locals['val'])
    return proc.r_rp


def prim_qt_qcombobox_new(proc):
    parent = proc.locals['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = QtGui.QComboBox(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QComboBox()
    return proc.r_rp

def prim_qt_qcombobox_add_item(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.addItem(proc.locals['item'])
    return proc.r_rp

def prim_qt_qcombobox_set_current_index(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setCurrentIndex(proc.locals['i'])
    return proc.r_rp

def prim_qt_qcombobox_clear(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.clear()
    return proc.r_rp

## QTableWidget

def prim_qt_qtablewidget_new(proc):
    parent = proc.locals['parent']
    rows = proc.locals['rows']
    cols = proc.locals['cols']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = QtGui.QTableWidget(rows, cols, qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QTableWidget(rows, cols)
    return proc.r_rp

def prim_qt_qtablewidget_set_horizontal_header_labels(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setHorizontalHeaderLabels(proc.locals['labels'])
    return proc.r_rp

def prim_qt_qtablewidget_vertical_header(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    header = qtobj.verticalHeader()
    QHeaderViewClass = proc.r_mp['QHeaderView']
    return proc.interpreter.alloc_object(QHeaderViewClass, {'self':header})

def prim_qt_qtablewidget_set_selection_mode(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setSelectionMode(proc.locals['mode'])
    return proc.r_rp

def prim_qt_qtablewidget_horizontal_header(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    header = qtobj.horizontalHeader()
    QHeaderViewClass = proc.r_mp['QHeaderView']
    return proc.interpreter.alloc_object(QHeaderViewClass, {'self':header})

def prim_qt_qtablewidget_set_item(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    item = _lookup_field(proc, proc.locals['item'], 'self')
    qtobj.setItem(proc.locals['line'], proc.locals['col'], item)
    return proc.r_rp

def prim_qt_qtablewidget_set_sorting_enabled(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setSortingEnabled(proc.locals['val'])
    return proc.r_rp

def prim_qt_qtablewidget_clear(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.clear()
    return proc.r_rp

# QTableWidgetItem

def prim_qt_qtablewidgetitem_new(proc):
    proc.r_rdp['self'] = QtGui.QTableWidgetItem(proc.locals['label'])
    return proc.r_rp

def prim_qt_qtablewidgetitem_set_flags(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setFlags(proc.locals['flags'])
    return proc.r_rp


def prim_qt_qwebview_new(proc):
    parent = proc.locals['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = QtWebKit.QWebView(qtobj)
    else:
        proc.r_rdp['self'] = QtWebKit.QWebView()
    return proc.r_rp

def prim_qt_qwebview_set_url(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setUrl(QtCore.QUrl(proc.locals['url']))
    return proc.r_rp

# note: this is not from Qt
# def prim_qt_qwebview_load_url(proc):
#     qtobj = _lookup_field(proc, proc.r_rp, 'self')
#     qtobj.setHtml(open(proc.locals['url']).read())
#     return proc.r_rp

def prim_qt_qwebview_set_html(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setHtml(proc.locals['html'])
    return proc.r_rp

def prim_qt_qwebview_page(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return _meme_instance(proc,qtobj.page())

def prim_qt_qwebpage_set_link_delegation_policy(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setLinkDelegationPolicy(proc.locals['policy'])
    return proc.r_rp

def prim_qt_qwebpage_main_frame(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return _meme_instance(proc,qtobj.mainFrame())

def prim_qt_qwebframe_document_element(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    x = _meme_instance(proc,qtobj.documentElement())
    return x

def prim_qt_qwebframe_scroll_to_anchor(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qtobj.scrollToAnchor(proc.locals['anchor'])

def prim_qt_qwebelement_find_first(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return _meme_instance(proc,qtobj.findFirst(proc.locals['str']))

def prim_qt_qwebelement_append_outside(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    if isinstance(proc.locals['val'], basestring):
        qtobj.appendOutside(proc.locals['val'])
    else:
        qtobj.appendOutside(_lookup_field(proc, proc.locals['val'], 'self'))
    return proc.r_rp

def prim_qt_qwebelement_append_inside(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    if isinstance(proc.locals['val'], basestring):
        qtobj.appendInside(proc.locals['val'])
    else:
        qtobj.appendInside(_lookup_field(proc, proc.locals['val'], 'self'))
    return proc.r_rp

def prim_qt_qwebelement_set_plain_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setPlainText(proc.locals['str'])
    return proc.r_rp

def prim_qt_qwebelement_clone(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return _meme_instance(proc,qtobj.clone())

def prim_qt_qwebelement_set_style_property(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setStyleProperty(proc.locals['name'], proc.locals['val'])
    return proc.r_rp

def prim_qt_qwebelement_set_attribute(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setAttribute(proc.locals['name'], proc.locals['val'])
    return proc.r_rp


def prim_qt_qwebelement_to_outer_xml(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.toOuterXml())

def prim_qt_qwebelement_set_inner_xml(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setInnerXml(proc.locals['xml'])
    return proc.r_rp

def prim_qt_qwebelement_take_from_document(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qtobj.takeFromDocument()

def prim_qt_qurl_has_fragment(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qtobj.hasFragment()

def prim_qt_qurl_fragment(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.fragment())

def prim_qt_qurl_query_item_value(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.queryItemValue(proc.locals['name']))

def prim_qt_qurl_has_query_item(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qtobj.hasQueryItem(proc.locals['name'])

def prim_qt_qurl_path(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.path())

def prim_qt_qurl_to_string(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.toString())



# scintilla
def prim_qt_scintilla_editor_new(proc):
    parent = proc.locals['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.r_rdp['self'] = scintilla_editor.MemeQsciScintilla(proc.r_rp, qtobj)
    else:
        proc.r_rdp['self'] = scintilla_editor.MemeQsciScintilla(proc.r_rp)
    return proc.r_rp

def prim_qt_scintilla_editor_set_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setText(proc.locals['text'])
    return proc.r_rp

def prim_qt_scintilla_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.text())


def prim_qt_scintilla_set_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setText(proc.locals['text'])
    return proc.r_rp

def prim_qt_scintilla_paused_at_line(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.paused_at_line(proc.locals['start_line'], proc.locals['start_col'], proc.locals['end_line'], proc.locals['end_col'])
    return proc.r_rp

def prim_qt_scintilla_selected_text(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    return qstring_to_str(qtobj.selectedText())

def prim_qt_scintilla_get_cursor_position(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    line, idx = qtobj.getCursorPosition()
    return {"line": line, "index":idx}

def prim_qt_scintilla_insert_at(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.insertAt(proc.locals['text'], proc.locals['line'], proc.locals['index'])
    return proc.r_rp

def prim_qt_scintilla_get_selection(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    start_line, start_index, end_line, end_index = qtobj.getSelection()
    return {"start_line":start_line, "start_index": start_index, 'end_line': end_line, 'end_index': end_index}

def prim_qt_scintilla_set_selection(proc):
    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    qtobj.setSelection(proc.locals['start_line'], proc.locals['start_index'], proc.locals['end_line'], proc.locals['end_index'])
    return proc.r_rp



# def prim_debugger_ui_new(proc):
#     qloop = _lookup_field(proc, proc.locals['qloop'], 'self')
#     proc = proc.locals['proc']
#     proc.r_rdp['self'] = dbgui.DebuggerUI(qloop,proc)
#     return proc.r_rp

# def prim_debugger_ui_show(proc):
#     qtobj = _lookup_field(proc, proc.r_rp, 'self')
#     qtobj.show()
#     return proc.r_rp

def prim_qapp_running(proc):
    global _qapp_running
    return  _qapp_running

######

_factories = []
def prim_qt_extra_qwebpage_enable_plugins(proc):
    name = proc.locals['name']
    fn = proc.locals['fn']
    class WebPluginFactory(QWebPluginFactory):
        def __init__(self, parent = None):
            QWebPluginFactory.__init__(self, parent)
        def create(self, mimeType, url, _names, _values):
            names = map(str, _names)
            values =  map(str, _values)
            if mimeType == "x-pyqt/" + name:
                try:
                    mobj = proc.setup_and_run_fun(None, None, '<?>', fn, [dict(zip(names,values))], True)
                    return _lookup_field(proc, mobj, 'self')
                except proc.interpreter.py_memetalk_exception() as e:
                    print "Exception raised: " + e.mmobj()['value']
                    print traceback.format_exc()

        def plugins(self):
            plugin = QWebPluginFactory.Plugin()
            plugin.name = "PyQt Widget"
            plugin.description = "An example Web plugin written with PyQt."
            mimeType = QWebPluginFactory.MimeType()
            mimeType.name = "x-pyqt/widget"
            mimeType.description = "PyQt widget"
            mimeType.fileExtensions = []
            plugin.mimeTypes = [mimeType]
            return [plugin]

    global _factories # If this is gc'd, it segfaults
    factory = WebPluginFactory()
    _factories.append(factory) #we may have many instances of qwebpage

    qtobj = _lookup_field(proc, proc.r_rp, 'self')
    QWebSettings.globalSettings().setAttribute(QWebSettings.PluginsEnabled, True)
    qtobj.setPluginFactory(factory)
    return proc.r_rp

def prim_test_files(proc):
    path = MODULES_PATH + "/../../tests"
    return [path + "/" + f for f in listdir(path) if isfile(join(path,f))]

def prim_test_import(proc):
    cmod = proc.interpreter.compiled_module_by_filepath(proc.locals['filepath'])
    return proc.interpreter.instantiate_module(cmod, [], proc.r_cp['module'])


def prim_http_get(proc):
    import httplib2
    resp, content = httplib2.Http().request(proc.locals['url'])
    return content

def prim_parse_json(proc):
    import json
    return json.loads(proc.locals['str'])
