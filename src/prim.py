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

from PyQt4 import QtGui
import sys
import re
from pdb import set_trace as br

def P(obj, depth=1):
    if depth > 5:
        depth = None
    from pprint import pprint
    pprint(obj, None, 1, 80, depth)

def prim_basic_new(i):
    def create_instances(klass):
        p = None
        if klass["parent"] != None:
            p = create_instances(klass["parent"])
        fields = klass["compiled_class"]["fields"]
        name = klass["compiled_class"]["name"]
        return dict({"_vt": klass, "_delegate":p, "@tag": name + " instance"}.items() + [(x,None) for x in fields])

    return create_instances(i.r_rp)

def prim_import(i):
    compiled_module = i.module_loader.load_module(i.stack[-1]["mname"])
    args = dict(zip(compiled_module["params"],i.stack[-1]["margs"]))
    imodule = i.instantiate_module(compiled_module, args, i.kernel_module_instance())
    return imodule

def prim_print(i):
    print(i.stack[-1]["arg"])

def prim_object_to_string(i):
    obj = i.r_rp
    if obj == None:
        return "null"
    elif isinstance(obj, basestring):
        return obj
    else:
        return str(obj)

# def prim_string_replace(i):
#     return re.sub(i.stack[-1]['what'],i.stack[-1]['for'],i.r_rp)

def prim_string_size(i):
    return len(i.r_rp)

def prim_class_compiled_function(i):
    return i.get_class("CompiledFunction")

def prim_compiled_function_new(i):
    #init new(text, parameters, module, env_idx_table_or_somethin')
    #  --we are only interested in the names of the env slots here
    cfun = i.compile_code(i.stack[-1]['text'],
                          i.stack[-1]['parameters'],
                          i.stack[-1]['module'],
                          i.stack[-1]['env'])
    return cfun

def prim_compiled_function_as_context(i):
    #asContext(imodule, env)
    # -- now we want the names and values of the env
    env = i.stack[-1]['env']
    if env != None:
        env = dict(zip(range(0,len(env.keys())), env.values()))
    else:
        env = None
    ret = i.compiled_function_to_context(i.r_rp, env, i.stack[-1]['imodule'])
    return ret

def prim_context_apply(i):
    # fn.apply([...args...])
    args = i.stack[-1]['args']
    return i.setup_and_run_fun(None, None, i.r_rdp, args, True)

def prim_context_get_env(i):
    #warning 1: no checking if the env idexes are ordered. we assume so.
    #warning 2: only the outter env is returned (closures declaring variables
    #                                            are not contemplated. its
    #                                            a TODO).
    env = i.r_rdp['env']
    env_table = i.r_rdp['compiled_function']['env_table']
    return dict(zip(env_table.values(),env.values()))

def prim_number_plus(i):
    return i.r_rp + i.stack[-1]['arg']

def prim_dictionary_plus(i):
    return dict(i.r_rp.items() + i.stack[-1]['arg'].items())

def prim_get_current_compiled_module(i):
    return i.get_vt(i.r_mp)['compiled_module']


# lookup the inner handle of the actual binding object
# which is set -- this is because a hierarchy of delegates
# will have the same field (say, QMainWindow < QWidget both
# have 'self', but QMainWindow.new() sets 'self' only to
# its leaf object, not to the delegated QWidget
def _lookup_field(mobj, name):
    if mobj == None:
        raise Exception('field not found: ' + name)
    if name in mobj and mobj[name] != None:
        return mobj[name]
    else:
        return _lookup_field(mobj['_delegate'], name)

# def _set_field(mobj, name, value):
#     if mobj == None:
#         raise Exception('field not found: ' + name)
#     if name in mobj:
#         mobj[name] = value
#     else:
#         _set_field(mobj['_delegate'], name, value)
### Qt

# QWidget
def prim_qt_qapplication_new(i):
    i.r_rdp['self'] = QtGui.QApplication(sys.argv)
    return i.r_rp

def prim_qt_qapplication_exec(i):
    return i.r_rdp['self'].exec_()

def prim_qt_qwidget_new(i):
    # new(QWidget parent)
    parent = i.stack[-1]['parent']
    if parent != None:
        qt_parent = _lookup_field(parent, 'self')
        i.r_rdp["self"] = QtGui.QWidget(qt_parent)
    else:
        i.r_rdp['self'] = QtGui.QWidget()
    return i.r_rp

def prim_qt_qwidget_show(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    qtobj.show()
    return i.r_rp

def prim_qt_qwidget_set_window_title(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    qtobj.setWindowTitle(i.stack[-1]['title'])
    return i.r_rp

def prim_qt_qwidget_resize(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    w = i.stack[-1]["w"]
    h = i.stack[-1]["h"]
    qtobj.resize(w,h)
    return i.r_rp

def prim_qt_qwidget_add_action(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    qt_action = _lookup_field(i.stack[-1]["action"], 'self')
    qtobj.addAction(qt_action)
    return i.r_rp

# QMainWindow
def prim_qt_qmainwindow_new(i):
    i.r_rdp['self'] = QtGui.QMainWindow()
    return i.r_rp

def prim_qt_qmainwindow_set_central_widget(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    qtobj.setCentralWidget(_lookup_field(i.stack[-1]['widget'],'self'))
    return i.r_rp

# Warning! QMenuBar inherits QWidget!
# however we did not create the QWidget instance delegate
# here.
def prim_qt_qmainwindow_menu_bar(i):
    QMenuBarClass = i.r_mp["QMenuBar"]
    qtobj = _lookup_field(i.r_rp, 'self')
    qt_bar_instance = qtobj.menuBar()
    return i.alloc(QMenuBarClass, {'self':qt_bar_instance})

# QPlainTextEdit
def prim_qt_qplaintextedit_new(i):
    # new(QWidget parent)
    parent = i.stack[-1]['parent']
    if parent != None:
        qt_parent = _lookup_field(parent, 'self')
    else:
        qt_parent = None
    i.r_rdp["self"] = QtGui.QPlainTextEdit(qt_parent)
    return i.r_rp

def prim_qt_qplaintextedit_set_tabstop_width(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    qtobj.setTabStopWidth(i.stack[-1]["val"])
    return i.r_rp

def prim_qt_qplaintextedit_text_cursor(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    qt_cursor = qtobj.textCursor()
    QTextCursorClass = i.r_mp["QTextCursor"]
    return i.alloc(QTextCursorClass, {'self':qt_cursor})

# QMenuBar
# Warning! QMenuBar inherits QWidget!
# however we did not create the QWidget instance delegate
# here.
def prim_qt_qmenubar_add_menu(i):
    label = i.stack[-1]["str"]
    qtobj = _lookup_field(i.r_rp, 'self')
    qt_menu_instance = qtobj.addMenu(label)
    QMenuClass = i.r_mp["QMenu"]
    return i.alloc(QMenuClass, {'self':qt_menu_instance})

# QAction
def prim_qt_qaction_new(i):
    label = i.stack[-1]['label']
    parent = i.stack[-1]['parent']
    if parent != None:
        qt_parent = _lookup_field(parent, 'self')
    else:
        qt_parent = None
    i.r_rdp["self"] = QtGui.QAction(label,qt_parent)
    return i.r_rp

def prim_qt_qaction_connect(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    signal = i.stack[-1]["signal"]
    slot = i.stack[-1]["slot"]
    def callback(*rest):
        i.setup_and_run_fun(None, None, slot, [], True)

    getattr(qtobj,signal).connect(callback)
    return i.r_rp

def prim_qt_qaction_set_shortcut(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    qtobj.setShortcut(i.stack[-1]["shortcut"]);
    return i.r_rp

# QTextCursor
def prim_qt_qtextcursor_selected_text(i):
    qtobj = _lookup_field(i.r_rp, 'self')
    string = str(qtobj.selectedText())
    return string
