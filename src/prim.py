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
    compiled_module = i.module_loader.load(i.stack[-1]["mname"])
    args = dict(zip(compiled_module["params"],i.stack[-1]["margs"]))
    imodule = _instantiate_module(compiled_module, args, _create_kernel_module_instance())
    return imodule

def prim_print(i):
    print(i.stack[-1]["arg"])


# def prim_callback(i):
#     cb = i.stack[-1]["cb"]
#     def callit():
#         i.setup_and_run_fun(None, None, cb, [], True)
#     return i.r_rp


# these won't be required when we have i.do_send()
# then, instead of lookup_field, and set field
# we execute i.do_send(recv, field_name) and
# i.do_send(recv, 'set_'+field_name, value)
def _lookup_field(mobj, name):
    if mobj == None:
        raise Exception('field not found: ' + name)
    if name in mobj:
        return mobj[name]
    else:
        return _lookup_field(mobj['_delegate'], name)

def _set_field(mobj, name, value):
    if mobj == None:
        raise Exception('field not found: ' + name)
    if name in mobj:
        mobj[name] = value
    else:
        _set_field(mobj['_delegate'], name, value)

### Qt

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
    i.r_rdp['self'].show()

# def primitive_qt_qwidget_set_window_title(vm, this, oop_string):
#     string = vm.get_text_from_string(oop_string)
#     vm.heap[this][1].setWindowTitle(string)
#     return this

# def primitive_qt_qwidget_resize(vm, this, oop_x, oop_y):
#     x = vm.strip_tag_int(oop_x)
#     y = vm.strip_tag_int(oop_y)
#     vm.heap[this][1].resize(x,y)
#     return this

# def primitive_qt_qwidget_hide(vm, this):
#     vm.heap[this][1].hide();
#     return this
