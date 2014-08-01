#include <QMetaType>
#include "process.hpp"
Q_DECLARE_METATYPE (Process*);

#include "qt_prims.hpp"
#include "vm.hpp"
#include "report.hpp"
#include "mmobj.hpp"
#include "core_image.hpp"
#include <QApplication>
#include <QWidget>
#include <QMetaType>
#include <QPushButton>
#include <QScriptEngine>



static
QScriptEngine *engine;

static const char* js_bridge =
  "function qt_bind_connect(qtobj, signal, mm_closure, process, handler_name) {"
  "  var global = this;"
  "  qtobj[signal].connect(qtobj, function() {"
  "    global[handler_name](process, mm_closure, arguments);"
  "  })"
  "}";

QScriptValue js_print(QScriptContext *ctx, QScriptEngine *engine) {
  QString str = ctx->argument(0).toString();
  std::cerr << "print: " << qPrintable(str) << endl;
  return engine->undefinedValue();
}

static
QScriptValue mm_handle0(QScriptContext *ctx, QScriptEngine *engine) {
  Process* proc = ctx->argument(0).toVariant().value<Process*>();
  oop mm_closure = (oop) ctx->argument(1).toVariant().value<void*>();

  // debug() << "QT: mm_handle0 " << button << " " << proc << " " << " " << mm_closure << endl;
  proc->do_call(mm_closure);
  return engine->undefinedValue();
}

static
void init_qt_stuff() { //stuff that needs QApplication to exist
  engine = new QScriptEngine;
  engine->evaluate(js_bridge, "<qt_prims>");
  QScriptValue globalObject = engine->globalObject();
  globalObject.setProperty("mm_handle_fun_0", engine->newFunction(mm_handle0), QScriptValue::SkipInEnumeration);
  globalObject.setProperty("print", engine->newFunction(js_print), QScriptValue::SkipInEnumeration);
}

///////////////////

static
oop lookup_bottom_qt_instance(MMObj* mmobj, oop obj) {
  if (obj == MM_NULL) {
    return NULL;
  } else if (mmobj->mm_object_vt(mmobj->mm_object_delegate(obj)) == mmobj->core()->get_prime("Object")) {
    return obj;
  } else {
    return lookup_bottom_qt_instance(mmobj, mmobj->mm_object_delegate(obj));
  }
}

static
void* get_qt_field(oop instance) {
  return ((oop*)instance)[2]; //@self
}

static
void set_qt_field(oop bottom_instance, void* qtobj) {
  ((oop*)bottom_instance)[2] = (oop) qtobj;
}


//////////

static int prim_qapplication_new(Process* proc) {
  oop data_self =  proc->dp();
  QApplication* app = new QApplication(proc->vm()->argc(), proc->vm()->argv());
  // debug() << "QT: QApplication " << app << endl;
  init_qt_stuff();
  set_qt_field(data_self, app);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qapplication_exec(Process* proc) {
  oop data_self =  proc->dp();
  QApplication* app = (QApplication*) get_qt_field(data_self);
  app->exec();
  proc->stack_push(proc->dp());
  return 0;
}

static int prim_qwidget_new(Process* proc) {
  oop data_self =  proc->dp();
  QWidget* w = new QWidget;
  // debug() << "QT: qwidget_new rp: " << proc->rp() << " dp: " << data_self << " widget: " << w << endl;
  set_qt_field(data_self, w);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qwidget_show(Process* proc) {
  oop data_self =  proc->dp();
  QWidget* w = (QWidget*) get_qt_field(data_self);
  w->show();
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qwidget_connect(Process* proc) {
  oop data_self =  proc->dp();
  QWidget* w = (QWidget*) get_qt_field(data_self);

  oop fn = *((oop*) proc->fp() - 1);
  oop signal = *((oop*) proc->fp() - 2);

  QScriptValueList args;
  args << engine->newQObject(w)
       << proc->mmobj()->mm_string_cstr(signal)
       << engine->newVariant(QVariant::fromValue((void*)fn))
       << engine->newVariant(QVariant::fromValue(proc))
       << "mm_handle_fun_0";

  QScriptValue globalObject = engine->globalObject();
  QScriptValue bridge = globalObject.property("qt_bind_connect");
  bridge.call(globalObject, args);
  if (engine->hasUncaughtException()) {
    debug() << "QT: exception: " << qPrintable(engine->uncaughtException().toString()) << endl;
    bail();
  }
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qpushbutton_new(Process* proc) {
  oop data_self =  proc->dp();

  oop oop_parent = *((oop*) proc->fp() - 1);
  oop oop_label = *((oop*) proc->fp() - 2);

  oop qt_data_parent = lookup_bottom_qt_instance(proc->mmobj(), oop_parent);
  QWidget* parent;
  if (qt_data_parent == NULL) {
    parent = NULL;
  } else {
    parent = (QWidget*) get_qt_field(qt_data_parent);
  }

  // debug() << "QT: prim_qpushbutton_new parent:" << oop_parent << " " << qt_data_parent << " " << parent << endl;
  QPushButton* button = new QPushButton(proc->mmobj()->mm_string_cstr(oop_label), parent);

  oop qt_data_self = lookup_bottom_qt_instance(proc->mmobj(), data_self);
  set_qt_field(qt_data_self, button);

  // debug() << "QT: prim_qpushbutton_new qt:" << button << " rp: " << proc->rp() << " dp: " << qt_data_self << " proc:" << proc << endl;

  proc->stack_push(proc->rp());
  return 0;
}

void qt_init_primitives(VM* vm) {
  vm->register_primitive("qt_qapplication_new", prim_qapplication_new);
  vm->register_primitive("qt_qapplication_exec", prim_qapplication_exec);

  vm->register_primitive("qt_qwidget_new", prim_qwidget_new);
  vm->register_primitive("qt_qwidget_show", prim_qwidget_show);
  vm->register_primitive("qt_qwidget_connect", prim_qwidget_connect);

  vm->register_primitive("qt_qpushbutton_new", prim_qpushbutton_new);
}
