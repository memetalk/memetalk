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
from pprint import pprint, pformat

def P(obj, depth=1):
    if depth > 5:
        depth = None
    from pprint import pprint
    pprint(obj, None, 1, 80, depth)

def prim_basic_new(proc):
    def create_instances(klass):
        p = None
        if klass["parent"] != None:
            p = create_instances(klass["parent"])
        fields = klass["compiled_class"]["fields"]
        name = klass["compiled_class"]["name"]
        return dict({"_vt": klass, "_delegate":p, "@tag": name + " instance"}.items() + [(x,None) for x in fields])

    return create_instances(proc.r_rp)

def prim_import(proc):
    compiled_module = proc.interpreter.module_loader.load_module(proc.stack[-1]["mname"])
    args = dict(zip(compiled_module["params"],proc.stack[-1]["margs"]))
    imodule = proc.interpreter.instantiate_module(compiled_module, args, proc.interpreter.kernel_module_instance())
    return imodule

def prim_print(proc):
    P(proc.stack[-1]["arg"],2)

def prim_object_to_string(proc):
    obj = proc.r_rp
    if obj == None:
        return "null"
    elif isinstance(obj, basestring):
        return obj
    else:
        return pformat(obj,1,80,1) #dirt and limited, str does inifinite loop

def prim_object_to_source(proc):
    return pformat(proc.r_rp, 1,80,2)


def prim_object_equal(proc):
    return proc.r_rp == proc.stack[-1]['other']

def prim_object_not_equal(proc):
    return proc.r_rp != proc.stack[-1]['other']

# def prim_string_replace(i):
#     return re.sub(i.stack[-1]['what'],i.stack[-1]['for'],i.r_rp)

def prim_string_size(proc):
    return len(proc.r_rp)

def prim_class_compiled_function(proc):
    return proc.interpreter.get_class("CompiledFunction")

def prim_compiled_function_new(proc):
    #init new(text, parameters, module, env_idx_table_or_somethin')
    #  --we are only interested in the names of the env slots here
    cfun = proc.interpreter.compile_code(proc.stack[-1]['text'],
                                         proc.stack[-1]['parameters'],
                                         proc.stack[-1]['module'],
                                         proc.stack[-1]['env'])
    return cfun

def prim_compiled_function_as_context(proc):
    #asContext(imodule, env)
    # -- now we want the names and values of the env
    env = proc.stack[-1]['env']
    this = proc.stack[-1]['self']
    if env != None:
        env = dict(zip(range(0,len(env.keys())), env.values()))
        env['r_rdp'] = env['r_rp'] = this
    elif this == None:
        env = None
    else:
        env = {'r_rdp': this, 'r_rp': this}
    ret = proc.interpreter.compiled_function_to_context(proc.r_rp, env, proc.stack[-1]['imodule'])
    return ret

def prim_context_apply(proc):
    # fn.apply([...args...])
    args = proc.stack[-1]['args']
    return proc.setup_and_run_fun(None, None, proc.r_rdp, args, True)

def prim_context_get_env(proc):
    #warning 1: no checking if the env idexes are ordered. we assume so.
    #warning 2: only the outter env is returned (closures declaring variables
    #                                            are not contemplated. its
    #                                            a TODO).
    env = proc.r_rdp['env']
    env_table = proc.r_rdp['compiled_function']['env_table']
    return dict(zip(env_table.values(),env.values()))

def prim_number_plus(proc):
    return proc.r_rp + proc.stack[-1]['arg']

def prim_dictionary_plus(proc):
    return dict(proc.r_rp.items() + proc.stack[-1]['arg'].items())

def prim_get_current_compiled_module(proc):
    return proc.get_vt(proc.r_mp)['compiled_module']



def prim_get_mirror_class(proc):
    return proc.interpreter.get_class("Mirror")

def prim_mirror_new(proc):
    proc.r_rdp["mirrored"] = proc.stack[-1]['mirrored']
    return proc.r_rp

def prim_mirror_fields(proc):
    mirrored = proc.r_rdp['mirrored']
    if hasattr(mirrored, '__iter__'):
        return mirrored.keys()
    else:
        return []

def prim_mirror_value_for(proc):
    return proc.r_rdp['mirrored'][proc.stack[-1]['name']]

def prim_mirror_set_value_for(proc):
    proc.r_rdp['mirrored'][proc.stack[-1]['name']] = proc.stack[-1]['value']

def prim_list_each(proc):
    for x in proc.r_rdp:
        proc.setup_and_run_fun(None, None, proc.stack[-1]['fn'], [x], True)

###


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

## return a meme instance given something from pyqt
def _meme_instance(proc, obj):
    mapping = {
        QtGui.QListWidgetItem: proc.r_mp["QListWidgetItem"]}

    if obj == None:
        return obj
    elif isinstance(obj, basestring):
        return obj# {"_vt":i.get_class("String"), 'self':obj}
    elif isinstance(obj, dict) and '_vt' not in obj:
        return obj#{"_vt":i.get_class("Dictionary"), 'self':obj}
    elif isinstance(obj, int) or isinstance(obj, long):
        return obj#{"_vt":i.get_class("Number"), 'self':obj}
    elif isinstance(obj, list):
        return obj#{"_vt":i.get_class("List"), 'self':obj}
    else: #should be a qt instance
        return {"_vt":mapping[obj.__class__], 'self':obj}


# QWidget
def prim_qt_qapplication_new(proc):
    global _app
    _app = QtGui.QApplication(sys.argv)
    proc.r_rdp['self'] = _app
    return proc.r_rp

def prim_qt_qapplication_exec(proc):
    return proc.r_rdp['self'].exec_()

def prim_qt_qwidget_new(proc):
    # new(QWidget parent)
    parent = proc.stack[-1]['parent']
    if parent != None:
        qt_parent = _lookup_field(parent, 'self')
        proc.r_rdp["self"] = QtGui.QWidget(qt_parent)
    else:
        proc.r_rdp['self'] = QtGui.QWidget()
    return proc.r_rp

def prim_qt_qwidget_show(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.show()
    return proc.r_rp

def prim_qt_qwidget_set_window_title(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.setWindowTitle(proc.stack[-1]['title'])
    return proc.r_rp

def prim_qt_qwidget_resize(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    w = proc.stack[-1]["w"]
    h = proc.stack[-1]["h"]
    qtobj.resize(w,h)
    return proc.r_rp

def prim_qt_qwidget_add_action(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qt_action = _lookup_field(proc.stack[-1]["action"], 'self')
    qtobj.addAction(qt_action)
    return proc.r_rp

def prim_qt_qwidget_set_maximum_width(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.setMaximumWidth(proc.stack[-1]['w'])
    return proc.r_rp

def prim_qt_qwidget_connect(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    signal = proc.stack[-1]['signal']
    slot = proc.stack[-1]['slot']
    def callback(*rest):
        args = []
        for arg in rest:
            args.append(_meme_instance(proc,arg))

        proc.interpreter.setup_and_run_fun(None, None, slot, args, True)
    getattr(qtobj,signal).connect(callback)
    return proc.r_rp

# QMainWindow
def prim_qt_qmainwindow_new(proc):
    proc.r_rdp['self'] = QtGui.QMainWindow()
    return proc.r_rp

def prim_qt_qmainwindow_set_central_widget(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.setCentralWidget(_lookup_field(proc.stack[-1]['widget'],'self'))
    return proc.r_rp

# Warning! QMenuBar inherits QWidget!
# however we did not create the QWidget instance delegate
# here.
def prim_qt_qmainwindow_menu_bar(proc):
    QMenuBarClass = proc.r_mp["QMenuBar"]
    qtobj = _lookup_field(proc.r_rp, 'self')
    qt_bar_instance = qtobj.menuBar()
    return proc.alloc(QMenuBarClass, {'self':qt_bar_instance})

# QPlainTextEdit
def prim_qt_qplaintextedit_new(proc):
    # new(QWidget parent)
    parent = proc.stack[-1]['parent']
    if parent != None:
        qt_parent = _lookup_field(parent, 'self')
    else:
        qt_parent = None
    proc.r_rdp["self"] = QtGui.QPlainTextEdit(qt_parent)
    return proc.r_rp

def prim_qt_qplaintextedit_set_tabstop_width(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.setTabStopWidth(proc.stack[-1]["val"])
    return proc.r_rp

def prim_qt_qplaintextedit_text_cursor(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qt_cursor = qtobj.textCursor()
    QTextCursorClass = proc.r_mp["QTextCursor"]
    return proc.alloc(QTextCursorClass, {'self':qt_cursor})

def prim_qt_qplaintextedit_set_text_cursor(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    cursor = _lookup_field(proc.stack[-1]['cursor'], 'self')
    qtobj.setTextCursor(cursor)
    return proc.r_rp

def prim_qt_qplaintextedit_set_plain_text(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.setPlainText(proc.stack[-1]['text'])
    return proc.r_rp


def prim_qt_qplaintextedit_to_plain_text(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    return str(qtobj.toPlainText())



# QMenuBar
# Warning! QMenuBar inherits QWidget!
# however we did not create the QWidget instance delegate
# here.
def prim_qt_qmenubar_add_menu(proc):
    label = proc.stack[-1]["str"]
    qtobj = _lookup_field(proc.r_rp, 'self')
    qt_menu_instance = qtobj.addMenu(label)
    QMenuClass = proc.r_mp["QMenu"]
    return proc.alloc(QMenuClass, {'self':qt_menu_instance})

# QAction
def prim_qt_qaction_new(proc):
    label = proc.stack[-1]['label']
    parent = proc.stack[-1]['parent']
    if parent != None:
        qt_parent = _lookup_field(parent, 'self')
    else:
        qt_parent = None
    proc.r_rdp["self"] = QtGui.QAction(label,qt_parent)
    return proc.r_rp

def prim_qt_qaction_connect(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    signal = proc.stack[-1]["signal"]
    slot = proc.stack[-1]["slot"]
    def callback(*rest):
        proc.setup_and_run_fun(None, None, slot, [], True)

    getattr(qtobj,signal).connect(callback)
    return proc.r_rp

def prim_qt_qaction_set_shortcut(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.setShortcut(proc.stack[-1]["shortcut"]);
    return proc.r_rp

# QTextCursor
def prim_qt_qtextcursor_selected_text(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    string = str(qtobj.selectedText())
    return string

def prim_qt_qtextcursor_selection_end(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    return qtobj.selectionEnd()

def prim_qt_qtextcursor_set_position(proc):
    pos = proc.stack[-1]['pos']
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.setPosition(pos)
    return proc.r_rp

def prim_qt_qtextcursor_insert_text(proc):
    text = proc.stack[-1]['text']
    string = text.decode('string_escape')
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.insertText(string)
    return proc.r_rp

def prim_qt_qtextcursor_drag_right(proc):
    length = proc.stack[-1]['len']
    qtobj = _lookup_field(proc.r_rp, 'self')
    res = qtobj.movePosition(
        QtGui.QTextCursor.Left, QtGui.QTextCursor.KeepAnchor, length)
    return True if res else False

#QLayout

def prim_qt_qlayout_add_widget(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    parent = _lookup_field(proc.stack[-1]['widget'], 'self')
    qtobj.addWidget(parent)
    return proc.r_rp

# QVBoxLayout
def prim_qt_qvboxlayout_new(proc):
    parent = proc.stack[-1]['parent']
    if parent != None:
        qtobj = _lookup_field(parent, 'self')
        proc.r_rdp['self'] = QtGui.QVBoxLayout(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QVBoxLayout()
    return proc.r_rp

def prim_qt_qvboxlayout_add_layout(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    parent = _lookup_field(proc.stack[-1]['layout'], 'self')
    qtobj.addLayout(parent)
    return proc.r_rp



# QHBoxLayout
def prim_qt_qhboxlayout_new(proc):
    parent = proc.stack[-1]['parent']
    if parent != None:
        qtobj = _lookup_field(parent, 'self')
        proc.r_rdp['self'] = QtGui.QHBoxLayout(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QHBoxLayout()
    return proc.r_rp


# QListWidget
def prim_qt_qlistwidget_new(proc):
    parent = proc.stack[-1]['parent']
    if parent != None:
        qtobj = _lookup_field(parent, 'self')
        proc.r_rdp['self'] = QtGui.QListWidget(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QListWidget()
    return proc.r_rp

def prim_qt_qlistwidget_current_item(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    return _meme_instance(proc,qtobj.currentItem())

# QListWidgetItem
def prim_qt_qlistwidgetitem_new(proc):
    txt = proc.stack[-1]['text']
    parent = proc.stack[-1]['parent']
    qtparent = None if parent == None else _lookup_field(parent, 'self')
    proc.r_rdp['self'] = QtGui.QListWidgetItem(txt,qtparent)
    return proc.r_rp

def prim_qt_qlistwidgetitem_text(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    return str(qtobj.text())


#QLineEdit
def prim_qt_qlineedit_new(proc):
    parent = proc.stack[-1]['parent']
    if parent != None:
        qtobj = _lookup_field(parent, 'self')
        proc.r_rdp['self'] = QtGui.QLineEdit(qtobj)
    else:
        proc.r_rdp['self'] = QtGui.QLineEdit()
    return proc.r_rp

def prim_qt_qlineedit_set_focus(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    qtobj.setFocus()
    return proc.r_rp

def prim_qt_qlineedit_text(proc):
    qtobj = _lookup_field(proc.r_rp, 'self')
    return str(qtobj.text())
