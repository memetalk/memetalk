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
import scintilla_editor
from config import MODULES_PATH
import traceback
from mmpprint import P
from dcmp import obj_eq, id_eq
import logging

logger = logging.getLogger("i")

_app = None
_qt_imodule = None

def prim_modules_path(proc):
    return MODULES_PATH

def prim_available_modules(proc):
    return [f[:-3] for f in listdir(MODULES_PATH) if isfile(join(MODULES_PATH,f)) and f != "core.md"]

def prim_get_module(proc):
    return proc.interpreter.compiled_module_by_filename(proc.locals()['name'] + ".mm")

def prim_save_module(proc):
    return proc.interpreter.save_module(proc.locals()['name'])

def prim_break_at(proc):
    cfun = proc.locals()['cfun']
    line = proc.locals()['line']
    proc.interpreter.break_at(cfun, line)
    return proc.reg('r_rdp')

def prim_import(proc):
    compiled_module = proc.interpreter.compiled_module_by_filename(proc.locals()["mname"])
    args = dict(zip(compiled_module["params"],proc.locals()["margs"]))
    imodule = proc.interpreter.instantiate_module(compiled_module, args, proc.interpreter.kernel_module_instance())
    return imodule

def prim_exception_throw(proc):
    proc.throw(proc.reg('r_rp'))

def prim_exception_type(proc):
    return proc.reg('r_rp')['_vt']

def prim_vmprocess_current(proc):
    VMProcess = proc.interpreter.get_core_class('VMProcess')
    VMStackFrameClass = proc.interpreter.get_core_class('VMStackFrame')
    frames = [proc.interpreter.alloc_object(VMStackFrameClass,{'self':x}) for x in proc.all_stack_frames()]
    return proc.interpreter.alloc_object(VMProcess, {'self': proc, 'id':proc.procid, 'frames': frames})

def prim_vmprocess_spawn(proc):
    logger.debug('prim_vmprocess_spawn')
    procid = proc.call_interpreter(True, 'spawn')
    logger.debug('prim_vmprocess_spawn got id:')
    proc.reg('r_rdp')['id'] = procid
    return proc.reg('r_rp')

def prim_vmprocess_exec_module(proc):
    logger.debug('prim_vmprocess_exec_module')
    mname = proc.locals()['mname']
    fname = proc.locals()['fname']
    args  = proc.locals()['args']
    procid = proc.call_interpreter(False, 'exec_module', proc.reg('r_rdp')['id'], mname, fname, args,)
    return proc.reg('r_rp')

def prim_vmprocess_debug(proc):
    logger.debug("prim_vmprocess_debug: firing up debugger")
    proc.call_interpreter(True, 'debug', proc.reg('r_rdp')['id'])

def prim_vmprocess_step_into(proc):
    raw_frames = proc.call_target_process(True, proc.reg('r_rdp')['id'], 'step_into')
    VMStackFrameClass = proc.interpreter.get_core_class('VMStackFrame')
    logger.debug('got frames, assemblying...')
    proc.reg('r_rdp')['frames'] = [proc.interpreter.alloc_object(VMStackFrameClass,{'self':x}) for x in raw_frames if x['r_cp']]

def prim_vmprocess_step_over(proc):
    raw_frames = proc.call_target_process(True, proc.reg('r_rdp')['id'], 'step_over')
    VMStackFrameClass = proc.interpreter.get_core_class('VMStackFrame')
    logger.debug('got frames, assemblying...')
    proc.reg('r_rdp')['frames'] = [proc.interpreter.alloc_object(VMStackFrameClass,{'self':x}) for x in raw_frames if x['r_cp']]

def prim_vmprocess_continue(proc):
    logger.debug('prim_vmprocess_continue')
    proc.call_target_process(True, proc.reg('r_rdp')['id'], 'continue')

def prim_vmprocess_update_object(proc):
    obj = proc.locals()['obj']
    logger.debug('prim_vmprocess_update_object')
    import pickle
    raw_obj = pickle.dumps(obj)
    proc.call_target_process(False, proc.reg('r_rdp')['id'], 'update_object', raw_obj)

def prim_vmprocess_reload_frame(proc):
    logger.debug('asking to reloading frame...')
    proc.call_target_process(False, proc.reg('r_rdp')['id'], 'reload_frame')

def prim_vmprocess_rewind_and_break(proc):
    logger.debug('asking to rewind...')
    frames_count = proc.locals()['frames_count']
    to_line = proc.locals()['to_line']
    proc.call_target_process(False, proc.reg('r_rdp')['id'], 'rewind_and_break', frames_count, to_line)

def prim_vmprocess_stack_frames(proc):
    logger.debug('prim_vmprocess_stack_frames')
    if proc.reg('r_rdp')['frames']:
        logger.debug('we have it cached, returning it')
        return proc.reg('r_rdp')['frames']
    else:
        logger.debug('asking stack frames...')
        raw_frames = proc.call_target_process(True, proc.reg('r_rdp')['id'], 'get_frames')
        VMStackFrameClass = proc.interpreter.get_core_class('VMStackFrame')
        logger.debug('got frames, assemblying...')
        proc.reg('r_rdp')['frames'] = [proc.interpreter.alloc_object(VMStackFrameClass,{'self':x}) for x in raw_frames if x['r_cp']]
        logger.debug('returning frames')
        return proc.reg('r_rdp')['frames']

def prim_vmprocess_eval(proc):
    logger.debug('asking to eval...')
    text = proc.locals()['text']
    frame_level = proc.locals()['frame_level'] + 1
    raw = proc.call_target_process(True, proc.reg('r_rdp')['id'], 'eval', text, frame_level)
    import pickle
    return pickle.loads(raw)

def prim_vmstackframe_instruction_pointer(proc):
    frame = _lookup_field(proc, proc.reg('r_rp'), 'self')
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


def prim_vmprocess_stop_on_exception(proc):
    proc.flag_stop_on_exception = True

def prim_vmstackframe_module_pointer(proc):
    frame = _lookup_field(proc, proc.reg('r_rp'), 'self')
    if frame['r_cp'] != None: #first frame is none
        return frame['r_cp']['module']
    else:
        return None

def prim_vmstackframe_context_pointer(proc):
    logger.debug('prim_vmstackframe_context_pointer')
    frame = _lookup_field(proc, proc.reg('r_rp'), 'self')
    logger.debug('got frame: returning:' + P(frame,1,True))
    return frame['r_cp']

def prim_vmstackframe_receiver_pointer(proc):
    frame = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return frame['r_rp']

def prim_vmstackframe_receiver_data_pointer(proc):
    frame = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return frame['r_rdp']

def prim_vmstackframe_environment_pointer(proc):
    frame = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return frame['r_ep']

def prim_vmstackframe_locals(proc):
    frame = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return frame['locals']

def prim_io_print(proc):
    arg = proc.locals()['arg']
    if isinstance(arg, dict):
        P(arg,4)
    else:
        print arg

def prim_ast_line(proc):
    br()

def prim_object_to_string(proc):
    obj = proc.reg('r_rp')
    if obj == None:
        return "null"
    elif isinstance(obj, dict) and '_vt' in obj:
        return P(obj,1, True) #dirt and limited, str does inifinite loop
    else:
        return str(obj)

def prim_object_to_source(proc):
    obj = proc.reg('r_rp')
    if obj == None:
        return 'null'
    elif isinstance(obj, dict) and '@tag' in obj:
        return  '<' + proc.reg('r_rp')['@tag'] + '>'
    else:
        return P(obj,1,True)

def prim_object_send(proc):
    selector_str = proc.locals()['selector']['self'] # should be a symbol
    args = proc.locals()['args']
    receiver = proc.reg('r_rp')
    return proc.do_send(receiver, selector_str, args)

def prim_object_equal(proc):
    return obj_eq(proc.reg('r_rp'), proc.locals()['other'])

def prim_object_not_equal(proc):
    return not obj_eq(proc.reg('r_rp'), proc.locals()['other'])

# def prim_string_replace(i):
#     return re.sub(i.locals()['what'],i.locals()['for'],i.reg('r_rp'))

def prim_string_size(proc):
    return len(proc.reg('r_rp'))

def prim_string_concat(proc):
    return proc.reg('r_rp') + proc.locals()['arg']

def prim_string_from(proc):
    return proc.reg('r_rp')[proc.locals()['idx']:]

def prim_string_rindex(proc):
    try:
        return proc.reg('r_rdp').rindex(proc.locals()['arg'])
    except ValueError:
        return -1

def prim_symbol_to_string(proc):
    return proc.reg('r_rdp')['self']

def prim_string_count(proc):
    return proc.reg('r_rp').count(proc.locals()['sub'])

def prim_string_substring(proc):
    return proc.reg('r_rp')[proc.locals()['from']:proc.locals()['from']+proc.locals()['count']]

def prim_string_split(proc):
    return proc.reg('r_rp').split(proc.locals()['sep'])

def prim_string_to_symbol(proc):
    return proc.interpreter.interned_symbol_for(proc.reg('r_rdp'))

def prim_string_char_code(proc):
    return ord(proc.reg('r_rdp'))

def prim_module_instance_compiled_module(proc):
    return proc.reg('r_rdp')['_vt']['compiled_module']

def prim_compiled_function_new_top_level(proc):
    name = proc.locals()['name']
    text = proc.locals()['text']
    owner = proc.locals()['owner']
    flag = proc.locals()['flag']
    cfun = proc.interpreter.compile_top_level(name,text,owner, flag)
    cfun['is_top_level'] = True
    return cfun

def _create_closure(proc, text, outer, is_embedded):
    cfun = proc.interpreter.compile_closure(text,outer)
    cfun['is_embedded'] = is_embedded #text is embeded in outter cfun?
    cfun['is_top_level'] = False
    return cfun

def prim_compiled_function_new_closure(proc):
    text = proc.locals()['text']
    outer = proc.locals()['outer_cfun']
    is_embedded = proc.locals()['is_embedded']
    return _create_closure(proc, text, outer, is_embedded)

def prim_compiled_function_set_code(proc):
    if proc.reg('r_rdp')['is_top_level']:
        return proc.interpreter.recompile_top_level(proc.reg('r_rdp'), proc.locals()['code'])
    else:
        # NOTE: if a Function of this cfun already exists, its env should
        # be updated, otherwise calling it will screw up var access.
        return proc.interpreter.recompile_closure(proc.reg('r_rdp'), proc.locals()['code'])



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

def _compiled_function_as_context_with_frame(cfun, proc, imod, frame):
    # If the frame passed has an r_ep, just use it as our env.
    # else, we need to construct a proxy env capable of changing
    # frame.locals(). We also need to patch r_rp function's env_table
    # so that all frame.locals()/this are mapped there (so env_lookup works).
    if frame['r_ep'] != None:
        env = frame['r_ep']
        # Though this cfun was already constructed with an outer_cfun,
        # now we have a frame, so it's cp should be our outer_cfun! :(
        cfun['outer_cfun'] = frame['r_cp']['compiled_function']
        # patching our env_table's indexes
        begin = max(cfun['outer_cfun']['env_table'].keys()) + 1
        cfun['env_table'] = dict([(x+begin,y) for x,y in cfun['env_table'].iteritems()])
    else:
        env = ProxyEnv(frame, cfun['env_table'])
        cfun['env_table'] = env.table # patching the env_table
    ret = proc.interpreter.compiled_function_to_context(cfun, env, imod)
    return ret

def prim_compiled_function_as_context_with_frame(proc):
    frame = proc.locals()['frame']
    imod = proc.locals()['imodule']
    return _compiled_function_as_context_with_frame(proc.reg('r_rp'), proc, imod, frame['self'])

def prim_compiled_function_as_context_with_vars(proc):
    var_dict = proc.locals()['vars']
    env = dict(var_dict) # we do 'del' below, lets no fuck with the parameter
    if 'this' in env: #another hack for binding this
        this = env['this']
        del env['this']
    else:
        this = None
    begin = len(proc.reg('r_rp')['env_table'])
    proc.reg('r_rp')['env_table'].update(dict(zip(range(begin,begin+len(env.keys())), env.keys())))
    env = dict([(key,env[name]) for key,name in proc.reg('r_rp')['env_table'].iteritems() if name in env])
    env['r_rdp'] = env['r_rp'] = this
    ret = proc.interpreter.compiled_function_to_context(proc.reg('r_rp'), env, proc.locals()['imodule'])
    return ret

def prim_compiled_function_instantiate(proc):
    return proc.interpreter.create_function_from_cfunction(proc.reg('r_rp'), proc.locals()['imodule'])

def prim_context_apply(proc):
    # fn.apply([...args...])
    args = proc.locals()['args']
    return proc.setup_and_run_fun(None, None, proc.reg('r_rdp')['compiled_function']['name'], proc.reg('r_rdp'), args, True)

def prim_context_get_env(proc):
    #warning 1: no checking if the env idexes are ordered. we assume so.
    #warning 2: only the outer env is returned (closures declaring variables
    #                                            are not contemplated. its
    #                                            a TODO).
    env = dict(proc.reg('r_rdp')['env'])
    env_table = dict(proc.reg('r_rdp')['compiled_function']['env_table'])
    ret = {}
    for k,v in env_table.items():
        if k in env:
            ret[v] = env[k]
    return ret

def prim_function_apply(proc):
    # fn.apply([...args...])
    args = proc.locals()['args']
    return proc.setup_and_run_fun(None, None, proc.reg('r_rdp')['compiled_function']['name'], proc.reg('r_rdp'), args, True)

def prim_number_plus(proc):
    return proc.reg('r_rp') + proc.locals()['arg']

def prim_number_minus(proc):
    return proc.reg('r_rp') - proc.locals()['arg']

def prim_number_lst(proc):
    return proc.reg('r_rp') < proc.locals()['arg']

def prim_number_lsteq(proc):
    return proc.reg('r_rp') <= proc.locals()['arg']

def prim_number_grteq(proc):
    return proc.reg('r_rp') >= proc.locals()['arg']

def prim_dictionary_plus(proc):
    return dict(proc.reg('r_rp').items() + proc.locals()['arg'].items())

def prim_dictionary_size(proc):
    return len(proc.reg('r_rp'))

def prim_dictionary_sorted_each(proc):
    d = proc.reg('r_rdp')
    dsorted = [(key,d[key]) for key in sorted(d.keys())]
    for t in dsorted:
        proc.setup_and_run_fun(None, None, 'fn', proc.locals()['fn'], [t[0],t[1]], True)

def prim_dictionary_each(proc):
    for key,val in proc.reg('r_rdp').iteritems():
        proc.setup_and_run_fun(None, None, 'fn', proc.locals()['fn'], [key,val], True)

def prim_dictionary_has(proc):
    return proc.locals()['key'] in proc.reg('r_rdp')

def prim_dictionary_set(proc):
    proc.reg('r_rdp')['key'] = proc.locals()["value"]

def prim_dictionary_remove(proc):
    del proc.reg('r_rdp')[proc.locals()['key']]

def prim_dictionary_map(proc):
    ret = []
    for key,val in proc.reg('r_rdp').iteritems():
        ret.append(proc.setup_and_run_fun(None, None, 'fn', proc.locals()['fn'], [key,val], True))
    return ret

def prim_get_compiled_module(proc):
    return proc.interpreter.get_vt(proc.locals()['module'])['compiled_module']

def prim_get_compiled_class(proc):
    return proc.locals()['klass']['compiled_class']

def prim_compiled_class_constructors(proc):
    return dict([(name, cfun) for name,cfun in proc.reg('r_rdp')['own_methods'].iteritems() if cfun['is_ctor']])

def prim_compiled_class_rename(proc):
    klass = proc.reg('r_rdp')
    new_name = proc.locals()['name']
    old_name = klass['name']
    cmod = klass['module']

    klass['name'] = new_name

    del cmod['compiled_classes'][old_name]
    cmod['compiled_classes'][new_name] = klass
    for imod in proc.interpreter.imodules():
        if id_eq(imod['_vt']['compiled_module'], cmod):
            iklass = imod[old_name]
            del imod[old_name]
            imod[new_name] = iklass
            del imod['_vt']['dict'][old_name] # del accessor
            imod['_vt']['dict'][new_name] = proc.interpreter.create_accessor_method(imod, new_name)
    return proc.reg('r_rp')

def prim_compiled_class_set_fields(proc):
    klass = proc.reg('r_rdp')
    fields = proc.locals()['fields']

    def diff(a, b):
        b = set(b)
        return [aa for aa in a if aa not in b]

    rm =  diff(klass['fields'], fields)
    add = diff(fields, klass['fields'])

    for obj in proc.interpreter.memory:
        if 'compiled_class' in obj['_vt'] and id_eq(obj['_vt']['compiled_class'], klass):
            for f in rm: del obj[f]
            for f in add: obj[f] = None
    klass['fields'] = fields


def prim_compiled_class_class_methods(proc):
    return dict([(name, cfun) for name,cfun in proc.reg('r_rdp')['own_methods'].iteritems() if not cfun['is_ctor']])

def prim_compiled_class_add_method(proc):
    cfun = proc.locals()['cfun']
    flag = proc.locals()['flag']

    # add the function to the compiled class
    if flag['self'] == 'instance_method':
        proc.reg('r_rdp')['methods'][cfun['name']] = cfun
    elif flag['self'] == 'class_method' or flag['self'] == 'constructor':
        proc.reg('r_rdp')['own_methods'][cfun['name']] = cfun
    else:
        proc.throw_with_message("Unknown flag: " + P(flag,1,True))

    # add the function to the instantiated classes:
    for imod in proc.interpreter.imodules():
        if id_eq(imod['_vt']['compiled_module'], proc.reg('r_rdp')['module']):
            fun = proc.interpreter.create_function_from_cfunction(cfun, imod)
            if flag['self'] == 'instance_method':
                imod[proc.reg('r_rdp')['name']]['dict'][cfun['name']] = fun
            else:
                imod[proc.reg('r_rdp')['name']]['_vt']['dict'][cfun['name']] = fun
    return proc.reg('r_rp')

def prim_compiled_class_remove_method(proc):
    name = proc.locals()['name']
    flag = proc.locals()['flag']
    cclass = proc.reg('r_rdp')

    # removing function from compiled class
    if flag['self'] == 'instance_method':
        del proc.reg('r_rdp')['methods'][name]
    else:
        del proc.reg('r_rdp')['own_methods'][name]

    # removing function from instances:
    for imod in proc.interpreter.imodules():
        if id_eq(imod['_vt']['compiled_module'], proc.reg('r_rdp')['module']):
            if flag['self'] == 'instance_method':
                del imod[proc.reg('r_rdp')['name']]['dict'][name]
            else:
                del imod[proc.reg('r_rdp')['name']]['_vt']['dict'][name]

def prim_compiled_class_method_flag(proc):
    if proc.locals()['cfun'] in proc.reg('r_rdp')['methods'].values():
        return proc.interpreter.interned_symbol_for("instance_method")
    elif proc.locals()['cfun'] in proc.reg('r_rdp')['own_methods'].values():
        return proc.interpreter.interned_symbol_for("class_method")
    return None

def prim_mirror_fields(proc):
    mirrored = proc.reg('r_rdp')['mirrored']
    if hasattr(mirrored, 'keys'):
        return mirrored.keys()
    elif isinstance(mirrored, list):
        return [str(x) for x in range(0,len(mirrored))]
    else:
        return []

def prim_mirror_value_for(proc):
    if isinstance(proc.reg('r_rdp')['mirrored'], list):
        return proc.reg('r_rdp')['mirrored'][int(proc.locals()['name'])]
    else:
        return proc.reg('r_rdp')['mirrored'][proc.locals()['name']]

def prim_mirror_set_value_for(proc):
    if isinstance(proc.reg('r_rdp')['mirrored'], list):
        proc.reg('r_rdp')['mirrored'][int(proc.locals()['name'])] = proc.locals()['value']
    else:
        proc.reg('r_rdp')['mirrored'][proc.locals()['name']] = proc.locals()['value']
    return proc.reg('r_rp')

def prim_mirror_vt(proc):
    return proc.interpreter.get_vt(proc.locals()['obj'])

def prim_list_ctor(proc):
    proc.reg('r_rdp').extend(proc.locals()['lst'])
    return proc.reg('r_rp')

def prim_list_each(proc):
    for x in proc.reg('r_rdp'):
        proc.setup_and_run_fun(None, None, 'fn', proc.locals()['fn'], [x], True)

def prim_list_first(proc):
    return proc.reg('r_rdp')[0]

def prim_list_rest(proc):
    return proc.reg('r_rdp')[1:]

def prim_list_reversed(proc):
    return list(reversed(proc.reg('r_rdp')))

def prim_list_prepend(proc):
    return proc.locals()['arg'] + proc.reg('r_rdp')

def prim_list_get(proc):
    return proc.reg('r_rdp')[proc.locals()['n']]

def prim_list_size(proc):
    return len(proc.reg('r_rdp'))

def prim_list_map(proc):
    return [proc.setup_and_run_fun(None, None, 'fn', proc.locals()['fn'], [x], True) for x in proc.reg('r_rdp')]

def prim_list_plus(proc):
    return list(proc.reg('r_rdp') + proc.locals()['arg'])

def prim_list_has(proc):
    return proc.locals()['value'] in proc.reg('r_rdp')

def prim_list_join(proc):
    return proc.locals()['sep'].join(proc.reg('r_rdp'))

def prim_list_to_string(proc):
    ret = []
    for x in proc.reg('r_rdp'):
        if hasattr(x,'__str__'):
            ret.append(str(x))
        else:
            ret.append(P(x,1,True))
    return ', '.join(ret)

def prim_io_file_contents(proc):
    return open(proc.locals()['path']).read()

def prim_compiled_module_remove_function(proc):
    name = proc.locals()['name']
    cmod = proc.reg('r_rdp')

    # removing function from compiled module
    del cmod['compiled_functions'][name]

    # removing function from instances:
    for imod in proc.interpreter.imodules():
        if id_eq(imod['_vt']['compiled_module'], cmod):
            # remove the getter...
            del imod['_vt']['dict'][name]
            # ...and the python dict field
            del imod[name]

def prim_compiled_module_add_function(proc):
    cfun = proc.locals()['cfun']
    cmod = proc.reg('r_rdp')

    # add the function to the compiled module
    cmod['compiled_functions'][cfun['name']] = cfun

    # add the function to the module instances:
    for imod in proc.interpreter.imodules():
        if id_eq(imod['_vt']['compiled_module'], cmod):
            fun = proc.interpreter.create_function_from_cfunction(cfun, imod)
            # add the getter
            imod['_vt']['dict'][cfun['name']] = fun
            # add the function to the dict:
            imod[cfun['name']] = fun

def prim_compiled_module_new_class(proc):
    name = proc.locals()['name']
    super_name = proc.locals()['super_name']
    cmod = proc.reg('r_rdp')
    klass = proc.interpreter.create_compiled_class({"name": name,
                                                    "super_class_name":super_name,
                                                    "module": cmod})

    cmod['compiled_classes'][name] = klass

    for imod in proc.interpreter.imodules():
        if id_eq(imod['_vt']['compiled_module'], cmod):
            cb = proc.interpreter.alloc({"_vt": proc.interpreter.get_core_class("Behavior"),
                                         "parent": proc.interpreter.get_core_class("ObjectBehavior")['_vt'],
                                         "dict": {},
                                         "@tag": name + "Behavior"})
            imod['_vt']['dict'][name] = proc.interpreter.create_accessor_method(imod, name)
            imod[name] = proc.interpreter.create_class({"_vt": cb,
                                                        "parent": proc.interpreter.get_core_class("Object"),
                                                        "dict": {},
                                                        "compiled_class": klass,
                                                        "@tag": name + " Class"})
    return klass

def prim_compiled_module_add_class(proc):
    klass = proc.locals()['klass']
    name = klass['name']
    cmod = proc.reg('r_rdp')
    klass['module'] = cmod

    cmod['compiled_classes'][name] = klass
    for imod in proc.interpreter.imodules():
        if id_eq(imod['_vt']['compiled_module'], cmod):
            cb = proc.interpreter.alloc({"_vt": proc.interpreter.get_core_class("Behavior"),
                                         "parent": proc.lookup_in_modules(klass["super_class_name"], imod)['_vt'],
                                         "dict": proc.interpreter.compiled_functions_to_functions(klass["own_methods"], imod),
                                         "@tag": name + "Behavior"})
            imod['_vt']['dict'][name] = proc.interpreter.create_accessor_method(imod, name)
            imod[name] = proc.interpreter.create_class({"_vt": cb,
                                                        "parent": proc.interpreter.get_core_class("Object"),
                                                        "dict": {},
                                                        "compiled_class": klass,
                                                        "@tag": name + " Class"})
    return klass


def prim_compiled_module_remove_class(proc):
    name = proc.locals()['name']
    cmod = proc.reg('r_rdp')
    del cmod['compiled_classes'][name]

    # removing function from instances:
    for imod in proc.interpreter.imodules():
        if id_eq(imod['_vt']['compiled_module'], cmod):
            del imod[name]
            del imod['_vt']['dict'][name]
    return proc.reg('r_rp')

def prim_compiled_module_default_parameter_for(proc):
    # really simplorious, for the moment
    if proc.locals()['name'] in proc.reg('r_rdp')['default_params']:
        return proc.reg('r_rdp')['default_params'][proc.locals()['name']]['value']
    else:
        return None

def prim_compiled_module_set_default_parameter(proc):
    # really simplorious, for the moment
    proc.reg('r_rdp')['default_params'][proc.locals()['name']] = \
        {'name': proc.locals()['name'], 'type':'lib','value':proc.locals()['m']}
    return proc.reg('r_rp')

def prim_compiled_module_instantiate(proc):
    core = proc.interpreter.get_core_module()
    return proc.interpreter.instantiate_module(proc.reg('r_rdp'), proc.locals()['args'], core)

# lookup the inner handle of the actual binding object
# which is set -- this is because a hierarchy of delegates
# will have the same field (say, QMainWindow < QWidget both
# have 'self', but QMainWindow.new() sets 'self' only to
# its leaf object, not to the delegated QWidget
def _lookup_field(proc, mobj, name):
    if mobj == None:
        proc.throw_with_message('field not found: ' + name)
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
        _QTableWidgetItem: _qt_imodule['QTableWidgetItem'],
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
            logger.debug("Returning meme_instance for " + str(obj))
            return obj.meme_instance
        else:
            return {"_vt":mapping[obj.__class__], 'self':obj}
    else:
        logger.warning("*** WARNING: object has no memetalk mapping specified:")
        logger.warning(P(obj,1,True))
        return None

def qstring_to_str(qstring):
    #return str(qstring.toUtf8()).decode("utf-8")
    return unicode(qstring.toUtf8(), "utf-8")


def prim_exit(proc):
    proc.exit(proc.locals()['code'])

#################### Qt bindings ####################


# QApplication

def prim_qt_qapplication_new(proc):
    global _qt_imodule
    _qt_imodule = proc.reg('r_mp')
    proc.reg('r_rdp')['self'] = QApplication(sys.argv)
    return proc.reg('r_rdp')

def prim_qt_qapplication_focus_widget(proc):
    w = QApplication.focusWidget()
    return _meme_instance(proc,w)

def prim_qt_qapplication_exit(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.exit(proc.locals()['code'])

def prim_qt_qapplication_exec(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qtobj.exec_()


def prim_qt_qeventloop_new(proc):
    qev = QEventLoop()
    proc.reg('r_rdp')['self'] = qev
    return proc.reg('r_rp')

def prim_qt_qeventloop_exec(proc):
    return proc.reg('r_rdp')['self'].exec_()

def prim_qt_qeventloop_exit(proc):
    return proc.reg('r_rdp')['self'].exit(proc.locals()['code'])

# QWidget

def prim_qt_qwidget_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qt_parent = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')["self"] = QtGui.QWidget(qt_parent)
    else:
        proc.reg('r_rdp')['self'] = QtGui.QWidget()
    return proc.reg('r_rp')


def prim_qt_qwidget_set_focus(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setFocus()
    return proc.reg('r_rp')


def prim_qt_qwidget_set_maximum_height(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setMaximumHeight(proc.locals()['h'])
    return proc.reg('r_rp')


def prim_qt_qwidget_set_minimum_size(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setMinimumSize(proc.locals()['w'],proc.locals()['h'])
    return proc.reg('r_rp')


def prim_qt_qwidget_set_minimum_width(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setMinimumWidth(proc.locals()['w'])
    return proc.reg('r_rp')



def prim_qt_qwidget_show(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.show()
    return proc.reg('r_rp')


def prim_qt_qwidget_hide(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.hide()
    return proc.reg('r_rp')


def prim_qt_qwidget_set_window_title(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setWindowTitle(proc.locals()['title'])
    return proc.reg('r_rp')


def prim_qt_qwidget_resize(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    w = proc.locals()["w"]
    h = proc.locals()["h"]
    qtobj.resize(w,h)
    return proc.reg('r_rp')


def prim_qt_qwidget_add_action(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qt_action = _lookup_field(proc, proc.locals()["action"], 'self')
    qtobj.addAction(qt_action)
    return proc.reg('r_rp')


def prim_qt_qwidget_set_maximum_width(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setMaximumWidth(proc.locals()['w'])
    return proc.reg('r_rp')


def prim_qt_qwidget_connect(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    signal = proc.locals()['signal']
    slot = proc.locals()['slot']
    def callback(*rest):
        args = []
        for arg in rest:
            args.append(_meme_instance(proc,arg))
        try:
            proc.setup_and_run_fun(None, None, '<?>', slot, args, True)
        except proc.interpreter.py_memetalk_exception() as e:
            logger.debug("prim_qt_qwidget_connect: Exception raised: " + e.mmobj()['message'])
            logger.debug("Python trace:")
            logger.debug(e.mmobj()['py_trace'])
            logger.debug("Memetalk trace:")
            logger.debug(e.mmobj()['mtrace'])

    getattr(qtobj,signal).connect(callback)
    return proc.reg('r_rp')


def prim_qt_qwidget_actions(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    r = [_meme_instance(proc, x) for x in qtobj.actions()]
    return r


def prim_qt_qwidget_set_stylesheet(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setStyleSheet(proc.locals()['s'])
    return proc.reg('r_rp')


def prim_qt_qwidget_is_visible(proc):
    return _lookup_field(proc, proc.reg('r_rp'), 'self').isVisible()


def prim_qt_qwidget_close(proc):
    return _lookup_field(proc, proc.reg('r_rp'), 'self').close()


def prim_qt_qwidget_has_focus(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
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
    proc.reg('r_rdp')['self'] = _QMainWindow(proc, proc.reg('r_rp'))
    return proc.reg('r_rp')


def prim_qt_qmainwindow_set_central_widget(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setCentralWidget(_lookup_field(proc, proc.locals()['widget'],'self'))
    return proc.reg('r_rp')

# Warning! QMenuBar inherits QWidget!
# however we did not create the QWidget instance delegate
# here.

def prim_qt_qmainwindow_menu_bar(proc):
    QMenuBarClass = proc.reg('r_mp')["QMenuBar"]
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qt_bar_instance = qtobj.menuBar()
    return proc.interpreter.alloc_object(QMenuBarClass, {'self':qt_bar_instance})


def prim_qt_qmainwindow_status_bar(proc):
    QWidgetClass = proc.reg('r_mp')["QWidget"]
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qt_bar_instance = qtobj.statusBar()
    return proc.interpreter.alloc_object(QWidgetClass, {'self':qt_bar_instance})

# QPlainTextEdit

def prim_qt_qplaintextedit_new(proc):
    # new(QWidget parent)
    parent = proc.locals()['parent']
    if parent != None:
        qt_parent = _lookup_field(proc, parent, 'self')
    else:
        qt_parent = None
    proc.reg('r_rdp')["self"] = QtGui.QPlainTextEdit(qt_parent)
    return proc.reg('r_rp')


def prim_qt_qplaintextedit_set_tabstop_width(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setTabStopWidth(proc.locals()["val"])
    return proc.reg('r_rp')


def prim_qt_qplaintextedit_text_cursor(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qt_cursor = qtobj.textCursor()
    QTextCursorClass = proc.reg('r_mp')["QTextCursor"]
    return proc.interpreter.alloc_object(QTextCursorClass, {'self':qt_cursor})


def prim_qt_qplaintextedit_set_text_cursor(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    cursor = _lookup_field(proc, proc.locals()['cursor'], 'self')
    qtobj.setTextCursor(cursor)
    return proc.reg('r_rp')


def prim_qt_qplaintextedit_set_plain_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setPlainText(proc.locals()['text'])
    return proc.reg('r_rp')



def prim_qt_qplaintextedit_to_plain_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.toPlainText())



# QMenuBar
# Warning! QMenuBar inherits QWidget!
# however we did not create the QWidget instance delegate
# here.

def prim_qt_qmenubar_add_menu(proc):
    label = proc.locals()["str"]
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qt_menu_instance = qtobj.addMenu(label)
    QMenuClass = proc.reg('r_mp')["QMenu"]
    return proc.interpreter.alloc_object(QMenuClass, {'self':qt_menu_instance})

# QAction

def prim_qt_qaction_new(proc):
    label = proc.locals()['label']
    parent = proc.locals()['parent']
    if parent != None:
        qt_parent = _lookup_field(proc, parent, 'self')
    else:
        qt_parent = None
    proc.reg('r_rdp')["self"] = QtGui.QAction(label,qt_parent)
    return proc.reg('r_rp')


def prim_qt_qaction_connect(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    signal = proc.locals()["signal"]
    slot = proc.locals()["slot"]
    def callback(*rest):
        try:
            proc.setup_and_run_fun(None, None, '<?>', slot, [], True)
        except proc.interpreter.py_memetalk_exception() as e:
            logger.debug("Exception raised: " + e.mmobj()['message'])
            logger.debug("Python trace:")
            logger.debug(e.mmobj()['py_trace'])
            logger.debug("Memetalk trace:")
            logger.debug(e.mmobj()['mtrace'])

    getattr(qtobj,signal).connect(callback)
    return proc.reg('r_rp')


def prim_qt_qaction_set_shortcut(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setShortcut(proc.locals()["shortcut"]);
    return proc.reg('r_rp')


def prim_qt_qaction_set_shortcut_context(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setShortcutContext(proc.locals()["context"]);
    return proc.reg('r_rp')


def prim_qt_qaction_set_enabled(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setEnabled(proc.locals()["val"]);
    return proc.reg('r_rp')


def prim_qt_qshortcut_set_context(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setContext(proc.locals()["context"]);
    return proc.reg('r_rp')


def prim_qt_qshortcut_new(proc):
    keys = proc.locals()['sc']
    parent = proc.locals()['parent']
    slot = proc.locals()['slot']

    if parent != None:
        qt_parent = _lookup_field(proc, parent, 'self')
    else:
        qt_parent = None

    proc.reg('r_rdp')["self"] = QtGui.QShortcut(keys,qt_parent)
    proc.reg('r_rdp')["self"].setKey(keys)

    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    def callback(*rest):
        proc.setup_and_run_fun(None, None, '<?>', slot, [], True)
        logger.debug('callback slot: ' + str(slot['compiled_function']['body']))
        logger.debug('callback: eventloop proc equal? ' + str(proc == eventloop_processes[-1]['proc']))

    qtobj.activated.connect(callback)
    return proc.reg('r_rp')

#     def callback():
#         proc.setup_and_run_fun(None, None, '<?>', fn, [], True)

#     shortcut = QtGui.QShortcut(QtGui.QKeySequence(keys), qtobj, callback)
#     shortcut.setContext(QtCore.Qt.WidgetShortcut)
#     return proc.reg('r_rp')

# QTextCursor


def prim_qt_qtextcursor_selected_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')

    # Qt docs say \u2029 is a paragraph, and should be replaced with \n
    # TODO: make this replace works written in memetalk instead of hardcoded
    # here.
    return qstring_to_str(qtobj.selectedText()).replace(u'\u2029', '\n')


def prim_qt_qtextcursor_selection_end(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qtobj.selectionEnd()


def prim_qt_qtextcursor_set_position(proc):
    pos = proc.locals()['pos']
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setPosition(pos)
    return proc.reg('r_rp')


def prim_qt_qtextcursor_insert_text(proc):
    text = proc.locals()['text']
    string = text.decode('string_escape')
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.insertText(string)
    return proc.reg('r_rp')


def prim_qt_qtextcursor_drag_right(proc):
    length = proc.locals()['len']
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    res = qtobj.movePosition(
        QtGui.QTextCursor.Left, QtGui.QTextCursor.KeepAnchor, length)
    return True if res else False

#QLayout


def prim_qt_qlayout_add_widget(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    parent = _lookup_field(proc, proc.locals()['widget'], 'self')
    qtobj.addWidget(parent)
    return proc.reg('r_rp')

# QVBoxLayout

def prim_qt_qvboxlayout_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = QtGui.QVBoxLayout(qtobj)
    else:
        proc.reg('r_rdp')['self'] = QtGui.QVBoxLayout()
    return proc.reg('r_rp')


def prim_qt_qvboxlayout_add_layout(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    parent = _lookup_field(proc, proc.locals()['layout'], 'self')
    qtobj.addLayout(parent)
    return proc.reg('r_rp')


def prim_qt_qvboxlayout_add_widget(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    w = _lookup_field(proc, proc.locals()['widget'], 'self')
    qtobj.addWidget(w)
    return proc.reg('r_rp')



# QHBoxLayout

def prim_qt_qhboxlayout_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = QtGui.QHBoxLayout(qtobj)
    else:
        proc.reg('r_rdp')['self'] = QtGui.QHBoxLayout()
    return proc.reg('r_rp')


def prim_qt_qhboxlayout_add_widget(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    w = _lookup_field(proc, proc.locals()['widget'], 'self')
    qtobj.addWidget(w)
    return proc.reg('r_rp')


def prim_qt_qhboxlayout_set_contents_margins(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setContentsMargins(proc.locals()['l'],proc.locals()['t'],proc.locals()['r'],proc.locals()['b'])
    return proc.reg('r_rp')

# QListWidget

def prim_qt_qlistwidget_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = QtGui.QListWidget(qtobj)
    else:
        proc.reg('r_rdp')['self'] = QtGui.QListWidget()
    return proc.reg('r_rp')


def prim_qt_qlistwidget_current_item(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return _meme_instance(proc,qtobj.currentItem())

# QListWidgetItem

def prim_qt_qlistwidgetitem_new(proc):
    txt = proc.locals()['text']
    parent = proc.locals()['parent']
    qtparent = None if parent == None else _lookup_field(proc, parent, 'self')
    proc.reg('r_rdp')['self'] = QtGui.QListWidgetItem(txt,qtparent)
    return proc.reg('r_rp')


def prim_qt_qlistwidgetitem_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.text())


#QLineEdit

def prim_qt_qlineedit_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = QtGui.QLineEdit(qtobj)
    else:
        proc.reg('r_rdp')['self'] = QtGui.QLineEdit()
    return proc.reg('r_rp')


def prim_qt_qlineedit_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.text())


def prim_qt_qlineedit_set_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setText(proc.locals()['text'])
    return proc.reg('r_rp')


def prim_qt_qlineedit_selected_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.selectedText())


def prim_qt_qlineedit_select_all(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.selectAll()
    return proc.reg('r_rp')


def prim_qt_qlineedit_set_selection(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setSelection(proc.locals()['start'], proc.locals()['length'])
    return proc.reg('r_rp')



def prim_qt_qlabel_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = QtGui.QLabel(qtobj)
    else:
        proc.reg('r_rdp')['self'] = QtGui.QLabel()
    return proc.reg('r_rp')


def prim_qt_qlabel_set_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setText(proc.locals()['text'])
    return proc.reg('r_rp')


def prim_qt_qheaderview_hide(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.hide()
    return proc.reg('r_rp')


def prim_qt_qheaderview_set_stretch_last_section(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setStretchLastSection(proc.locals()['val'])
    return proc.reg('r_rp')



def prim_qt_qcombobox_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = QtGui.QComboBox(qtobj)
    else:
        proc.reg('r_rdp')['self'] = QtGui.QComboBox()
    return proc.reg('r_rp')


def prim_qt_qcombobox_add_item(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.addItem(proc.locals()['item'])
    return proc.reg('r_rp')


def prim_qt_qcombobox_set_current_index(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setCurrentIndex(proc.locals()['i'])
    return proc.reg('r_rp')


def prim_qt_qcombobox_clear(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.clear()
    return proc.reg('r_rp')

def prim_qt_qcombobox_count(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qtobj.count()

## QTableWidget


def prim_qt_qtablewidget_new(proc):
    parent = proc.locals()['parent']
    #rows = proc.locals()['rows']
    #cols = proc.locals()['cols']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = QtGui.QTableWidget(qtobj)
    else:
        proc.reg('r_rdp')['self'] = QtGui.QTableWidget()
    return proc.reg('r_rp')


def prim_qt_qtablewidget_set_horizontal_header_labels(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setHorizontalHeaderLabels(proc.locals()['labels'])
    return proc.reg('r_rp')


def prim_qt_qtablewidget_vertical_header(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    header = qtobj.verticalHeader()
    QHeaderViewClass = proc.reg('r_mp')['QHeaderView']
    return proc.interpreter.alloc_object(QHeaderViewClass, {'self':header})


def prim_qt_qtablewidget_set_selection_mode(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setSelectionMode(proc.locals()['mode'])
    return proc.reg('r_rp')


def prim_qt_qtablewidget_horizontal_header(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    header = qtobj.horizontalHeader()
    QHeaderViewClass = proc.reg('r_mp')['QHeaderView']
    return proc.interpreter.alloc_object(QHeaderViewClass, {'self':header})


def prim_qt_qtablewidget_set_item(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    item = _lookup_field(proc, proc.locals()['item'], 'self')
    qtobj.setItem(proc.locals()['line'], proc.locals()['col'], item)
    return proc.reg('r_rp')


def prim_qt_qtablewidget_set_sorting_enabled(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setSortingEnabled(proc.locals()['val'])
    return proc.reg('r_rp')


def prim_qt_qtablewidget_clear(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.clear()
    return proc.reg('r_rp')


def prim_qt_qtablewidget_set_row_count(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setRowCount(proc.locals()['count'])
    return proc.reg('r_rp')


def prim_qt_qtablewidget_set_column_count(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setColumnCount(proc.locals()['count'])
    return proc.reg('r_rp')


# QTableWidgetItem
class _QTableWidgetItem(QTableWidgetItem):
    def __init__(self, meme, label):
        super(QTableWidgetItem, self).__init__(label)
        self.meme_instance = meme


def prim_qt_qtablewidgetitem_new(proc):
    proc.reg('r_rdp')['self'] = _QTableWidgetItem(proc.reg('r_rp'), proc.locals()['label'])
    return proc.reg('r_rp')


def prim_qt_qtablewidgetitem_set_flags(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setFlags(Qt.ItemFlag(proc.locals()['flags']))
    return proc.reg('r_rp')



def prim_qt_qwebview_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = QtWebKit.QWebView(qtobj)
    else:
        proc.reg('r_rdp')['self'] = QtWebKit.QWebView()
    return proc.reg('r_rp')


def prim_qt_qwebview_set_url(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setUrl(QtCore.QUrl(proc.locals()['url']))
    return proc.reg('r_rp')

# note: this is not from Qt
# def prim_qt_qwebview_load_url(proc):
#     qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
#     qtobj.setHtml(open(proc.locals()['url']).read())
#     return proc.reg('r_rp')


def prim_qt_qwebview_set_html(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setHtml(proc.locals()['html'])
    return proc.reg('r_rp')


def prim_qt_qwebview_page(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return _meme_instance(proc,qtobj.page())


def prim_qt_qwebpage_set_link_delegation_policy(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setLinkDelegationPolicy(proc.locals()['policy'])
    return proc.reg('r_rp')


def prim_qt_qwebpage_main_frame(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return _meme_instance(proc,qtobj.mainFrame())


def prim_qt_qwebframe_document_element(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    x = _meme_instance(proc,qtobj.documentElement())
    return x


def prim_qt_qwebframe_scroll_to_anchor(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qtobj.scrollToAnchor(proc.locals()['anchor'])


def prim_qt_qwebelement_find_first(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return _meme_instance(proc,qtobj.findFirst(proc.locals()['str']))


def prim_qt_qwebelement_append_outside(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    if isinstance(proc.locals()['val'], basestring):
        qtobj.appendOutside(proc.locals()['val'])
    else:
        qtobj.appendOutside(_lookup_field(proc, proc.locals()['val'], 'self'))
    return proc.reg('r_rp')


def prim_qt_qwebelement_append_inside(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    if isinstance(proc.locals()['val'], basestring):
        qtobj.appendInside(proc.locals()['val'])
    else:
        qtobj.appendInside(_lookup_field(proc, proc.locals()['val'], 'self'))
    return proc.reg('r_rp')


def prim_qt_qwebelement_set_plain_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setPlainText(proc.locals()['str'])
    return proc.reg('r_rp')


def prim_qt_qwebelement_clone(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return _meme_instance(proc,qtobj.clone())


def prim_qt_qwebelement_set_style_property(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setStyleProperty(proc.locals()['name'], proc.locals()['val'])
    return proc.reg('r_rp')


def prim_qt_qwebelement_set_attribute(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setAttribute(proc.locals()['name'], proc.locals()['val'])
    return proc.reg('r_rp')



def prim_qt_qwebelement_to_outer_xml(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.toOuterXml())


def prim_qt_qwebelement_set_inner_xml(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setInnerXml(proc.locals()['xml'])
    return proc.reg('r_rp')


def prim_qt_qwebelement_take_from_document(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qtobj.takeFromDocument()


def prim_qt_qurl_has_fragment(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qtobj.hasFragment()


def prim_qt_qurl_fragment(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.fragment())


def prim_qt_qurl_query_item_value(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.queryItemValue(proc.locals()['name']))


def prim_qt_qurl_has_query_item(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qtobj.hasQueryItem(proc.locals()['name'])


def prim_qt_qurl_path(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.path())


def prim_qt_qurl_to_string(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.toString())



# scintilla

def prim_qt_scintilla_editor_new(proc):
    parent = proc.locals()['parent']
    if parent != None:
        qtobj = _lookup_field(proc, parent, 'self')
        proc.reg('r_rdp')['self'] = scintilla_editor.MemeQsciScintilla(proc.reg('r_rp'), qtobj)
    else:
        proc.reg('r_rdp')['self'] = scintilla_editor.MemeQsciScintilla(proc.reg('r_rp'))
    return proc.reg('r_rp')


def prim_qt_scintilla_editor_set_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setText(proc.locals()['text'])
    return proc.reg('r_rp')


def prim_qt_scintilla_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.text())


def prim_qt_scintilla_cut(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.cut()
    return proc.reg('r_rp')


def prim_qt_scintilla_copy(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.copy()
    return proc.reg('r_rp')


def prim_qt_scintilla_paste(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.paste()
    return proc.reg('r_rp')


def prim_qt_scintilla_redo(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.redo()
    return proc.reg('r_rp')



def prim_qt_scintilla_set_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.set_text(proc.locals()['text'])
    return proc.reg('r_rp')


def prim_qt_scintilla_saved(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.saved()
    return proc.reg('r_rp')


def prim_qt_scintilla_paused_at_line(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.paused_at_line(proc.locals()['start_line'], proc.locals()['start_col'], proc.locals()['end_line'], proc.locals()['end_col'])
    return proc.reg('r_rp')


def prim_qt_scintilla_selected_text(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    return qstring_to_str(qtobj.selectedText())


def prim_qt_scintilla_get_cursor_position(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    line, idx = qtobj.getCursorPosition()
    return {"line": line, "index":idx}


def prim_qt_scintilla_insert_at(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.insertAt(proc.locals()['text'], proc.locals()['line'], proc.locals()['index'])
    return proc.reg('r_rp')


def prim_qt_scintilla_get_selection(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    start_line, start_index, end_line, end_index = qtobj.getSelection()
    return {"start_line":start_line, "start_index": start_index, 'end_line': end_line, 'end_index': end_index}


def prim_qt_scintilla_set_selection(proc):
    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    qtobj.setSelection(proc.locals()['start_line'], proc.locals()['start_index'], proc.locals()['end_line'], proc.locals()['end_index'])
    return proc.reg('r_rp')



_factories = []

def prim_qt_extra_qwebpage_enable_plugins(proc):
    name = proc.locals()['name']
    fn = proc.locals()['fn']
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
                    logger.debug("Exception raised: " + e.mmobj()['message'])
                    logger.debug("Python trace:")
                    logger.debug(e.mmobj()['py_trace'])
                    logger.debug("Memetalk trace:")
                    logger.debug(e.mmobj()['mtrace'])

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

    qtobj = _lookup_field(proc, proc.reg('r_rp'), 'self')
    QWebSettings.globalSettings().setAttribute(QWebSettings.PluginsEnabled, True)
    qtobj.setPluginFactory(factory)
    return proc.reg('r_rp')

def prim_test_files(proc):
    path = MODULES_PATH + "/../../tests"
    return [path + "/" + f for f in listdir(path) if isfile(join(path,f))]

def prim_exception_unprotected(proc):
    return proc.setup_and_run_unprotected(None, None, 'fn', proc.locals()['fn'], [], True)

def prim_test_import(proc):
    cmod = proc.interpreter.compiled_module_by_filepath(proc.locals()['filepath'])
    return proc.interpreter.instantiate_module(cmod, [], proc.reg('r_cp')['module'])


def prim_http_get(proc):
    import httplib2
    resp, content = httplib2.Http().request(proc.locals()['url'])
    return content

def prim_parse_json(proc):
    import json
    return json.loads(proc.locals['str'])
