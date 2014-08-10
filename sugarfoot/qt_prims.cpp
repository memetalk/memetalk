#include <QMetaType>
#include "process.hpp"
Q_DECLARE_METATYPE (Process*);

#include "qt_prims.hpp"
#include "vm.hpp"
#include "report.hpp"
#include "mmobj.hpp"
#include "utils.hpp"
#include "core_image.hpp"
#include <QScriptEngine>
#include <QApplication>
#include <QAction>
#include <QComboBox>

#include <QWidget>
#include <QMetaType>
#include <QPushButton>



static
QScriptEngine *engine;

static const char* js_bridge =
  "function qt_bind_connect(qtobj, signal, mm_closure, process) {"
  "  var global = this;"
  "  qtobj[signal].connect(qtobj, function() {"
  "    return mm_handle(process, mm_closure, arguments);"
  "  })"
  "}";

QScriptValue js_print(QScriptContext *ctx, QScriptEngine *engine) {
  QString str = ctx->argument(0).toString();
  std::cerr << "print: " << qPrintable(str) << endl;
  return engine->undefinedValue();
}

static
QScriptValue mm_handle(QScriptContext *ctx, QScriptEngine *engine) {
  Process* proc = ctx->argument(0).toVariant().value<Process*>();
  oop mm_closure = (oop) ctx->argument(1).toVariant().value<void*>();
  QScriptValue args = ctx->argument(2);

  oop mm_args = proc->mmobj()->mm_list_new_empty();
  for (int i = 0; i < args.property("length").toInt32(); i++) {
    QScriptValue arg = args.property(i);
    if (arg.isBool()) {
      proc->mmobj()->mm_list_append(mm_args, proc->mmobj()->mm_new_boolean(arg.toBool()));
    } else if (arg.isNull()) {
      proc->mmobj()->mm_list_append(mm_args, MM_NULL);
    } else if (arg.isNumber()) {
      bail("TODO: arg.toNumber");
    } else if (arg.isQObject()) {
      bail("TODO: arg.toQObject");
    } else {
      bail("TODO: unknown arg");
    }
  }
  proc->do_call(mm_closure, mm_args);
  return engine->undefinedValue();
}

static
void init_qt_stuff() { //stuff that needs QApplication to exist
  engine = new QScriptEngine;
  engine->evaluate(js_bridge, "<qt_prims>");
  QScriptValue globalObject = engine->globalObject();
  globalObject.setProperty("mm_handle", engine->newFunction(mm_handle), QScriptValue::SkipInEnumeration);
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


static
void* get_qt_instance(MMObj* mmobj, oop obj) {
  oop bottom = lookup_bottom_qt_instance(mmobj, obj);
  if (bottom) {
    return get_qt_field(bottom);
  } else {
    return NULL;
  }
}

static
void set_qt_instance(MMObj* mmobj, oop obj, void* qtobj) {
  oop bottom = lookup_bottom_qt_instance(mmobj, obj);
  set_qt_field(bottom, qtobj);
}

////////// bindings /////////


/** QApplication **/

static int prim_qapplication_new(Process* proc) {
  oop data_self =  proc->dp();
  QApplication* app = new QApplication(proc->vm()->argc(), proc->vm()->argv());
  // debug() << "QT: QApplication " << app << endl;
  init_qt_stuff();
  set_qt_instance(proc->mmobj(), data_self, app);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qapplication_exec(Process* proc) {
  oop data_self =  proc->dp();
  QApplication* app = (QApplication*) get_qt_instance(proc->mmobj(), data_self);
  app->exec();
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qapplication_exit(Process* proc) {
  oop data_self =  proc->dp();
  oop code = *((oop*) proc->fp() - 1);
  QApplication* app = (QApplication*) get_qt_instance(proc->mmobj(), data_self);
  app->exit(untag_small_int(code));
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qapplication_focus_widget(Process* proc) {
  oop data_self =  proc->dp();
  QApplication* app = (QApplication*) get_qt_instance(proc->mmobj(), data_self);
  app->focusWidget();
  proc->stack_push(proc->rp());
  return 0;
}

/** QAction **/

static int prim_qaction_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_parent = *((oop*) proc->fp() - 1);
  oop oop_label = *((oop*) proc->fp() - 2);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QAction* qtobj = new QAction(proc->mmobj()->mm_string_cstr(oop_label), parent);
  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qaction_connect(Process* proc) {
  oop data_self =  proc->dp();
  QWidget* w = (QWidget*) get_qt_instance(proc->mmobj(), data_self);

  oop fn = *((oop*) proc->fp() - 1);
  oop signal = *((oop*) proc->fp() - 2);

  QScriptValueList args;
  args << engine->newQObject(w)
       << proc->mmobj()->mm_string_cstr(signal)
       << engine->newVariant(QVariant::fromValue((void*)fn))
       << engine->newVariant(QVariant::fromValue(proc));

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

static int prim_qaction_set_enabled(Process* proc) {
  oop data_self =  proc->dp();
  oop val = *((oop*) proc->fp() - 1);
  QAction* qtobj = (QAction*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->setEnabled(proc->mmobj()->mm_bool(val));
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qaction_set_shortcut(Process* proc) {
  oop data_self =  proc->dp();
  oop shortcut = *((oop*) proc->fp() - 1);
  QAction* qtobj = (QAction*) get_qt_instance(proc->mmobj(), data_self);

  qtobj->setShortcut(QString(proc->mmobj()->mm_string_cstr(shortcut)));
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qaction_set_shortcut_context(Process* proc) {
  oop data_self =  proc->dp();
  oop ctx = *((oop*) proc->fp() - 1);
  QAction* qtobj = (QAction*) get_qt_instance(proc->mmobj(), data_self);

  qtobj->setShortcutContext((Qt::ShortcutContext) untag_small_int(ctx));
  proc->stack_push(proc->rp());
  return 0;
}



/** QComboBox **/

static int prim_qt_qcombobox_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_parent = *((oop*) proc->fp() - 1);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);
  QComboBox* qtobj = new QComboBox(parent);

  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qcombobox_add_item(Process* proc) {
  oop data_self =  proc->dp();
  oop item = *((oop*) proc->fp() - 1);

  QComboBox* qtobj = (QComboBox*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->addItem(proc->mmobj()->mm_string_cstr(item));
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qcombobox_clear(Process* proc) {
  oop data_self =  proc->dp();

  QComboBox* qtobj = (QComboBox*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->clear();
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qcombobox_count(Process* proc) {
  oop data_self =  proc->dp();

  QComboBox* qtobj = (QComboBox*) get_qt_instance(proc->mmobj(), data_self);
  proc->stack_push((oop) tag_small_int(qtobj->count()));
  return 0;
}

static int prim_qt_qcombobox_set_current_index(Process* proc) {
  oop data_self =  proc->dp();
  oop idx = *((oop*) proc->fp() - 1);

  QComboBox* qtobj = (QComboBox*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->setCurrentIndex(untag_small_int(idx));
  proc->stack_push(proc->rp());
  return 0;
}




////////////


static int prim_qwidget_new(Process* proc) {
  oop data_self =  proc->dp();
  QWidget* w = new QWidget;
  // debug() << "QT: qwidget_new rp: " << proc->rp() << " dp: " << data_self << " widget: " << w << endl;
  set_qt_instance(proc->mmobj(), data_self, w);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qwidget_show(Process* proc) {
  oop data_self =  proc->dp();
  QWidget* w = (QWidget*) get_qt_instance(proc->mmobj(), data_self);
  w->show();
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qwidget_connect(Process* proc) {
  oop data_self =  proc->dp();
  QWidget* w = (QWidget*) get_qt_instance(proc->mmobj(), data_self);

  oop fn = *((oop*) proc->fp() - 1);
  oop signal = *((oop*) proc->fp() - 2);

  QScriptValueList args;
  args << engine->newQObject(w)
       << proc->mmobj()->mm_string_cstr(signal)
       << engine->newVariant(QVariant::fromValue((void*)fn))
       << engine->newVariant(QVariant::fromValue(proc));

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




//// temporary

static int prim_qpushbutton_new(Process* proc) {
  oop data_self =  proc->dp();

  oop oop_parent = *((oop*) proc->fp() - 1);
  oop oop_label = *((oop*) proc->fp() - 2);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QPushButton* button = new QPushButton(proc->mmobj()->mm_string_cstr(oop_label), parent);

  set_qt_instance(proc->mmobj(), data_self, button);
  proc->stack_push(proc->rp());
  return 0;
}

void qt_init_primitives(VM* vm) {
  vm->register_primitive("qt_qapplication_new", prim_qapplication_new);
  vm->register_primitive("qt_qapplication_exec", prim_qapplication_exec);
  vm->register_primitive("qt_qapplication_exit", prim_qapplication_exit);
  vm->register_primitive("qt_qapplication_focus_widget", prim_qapplication_focus_widget);

  vm->register_primitive("qt_qaction_new", prim_qaction_new);
  vm->register_primitive("qt_qaction_connect", prim_qaction_connect);
  vm->register_primitive("qt_qaction_set_enabled", prim_qaction_set_enabled);
  vm->register_primitive("qt_qaction_set_shortcut", prim_qaction_set_shortcut);
  vm->register_primitive("qt_qaction_set_shortcut_context", prim_qaction_set_shortcut_context);

  vm->register_primitive("qt_qcombobox_new", prim_qt_qcombobox_new);
  vm->register_primitive("qt_qcombobox_add_item", prim_qt_qcombobox_add_item);
  vm->register_primitive("qt_qcombobox_clear", prim_qt_qcombobox_clear);
  vm->register_primitive("qt_qcombobox_count", prim_qt_qcombobox_count);
  vm->register_primitive("qt_qcombobox_set_current_index", prim_qt_qcombobox_set_current_index);

  ///


  vm->register_primitive("qt_qwidget_new", prim_qwidget_new);
  vm->register_primitive("qt_qwidget_show", prim_qwidget_show);
  vm->register_primitive("qt_qwidget_connect", prim_qwidget_connect);

  vm->register_primitive("qt_qpushbutton_new", prim_qpushbutton_new);
}
