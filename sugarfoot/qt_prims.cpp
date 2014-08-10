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
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QListWidgetitem>
#include <QMainWindow>
#include <QMenuBar>
#include <QStatusBar>

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

static
void set_meme_instance(QObject* obj, oop instance) {
  obj->setProperty("meme_instance", QVariant::fromValue((void*)instance));
}

static
oop meme_instance(Process* proc, QObject* obj) {
  QVariant prop = obj->property("meme_instance");
  if (prop.isValid()) {
    return (oop) prop.value<void*>();
  } else {
    oop qt_imod = proc->mp();
    oop qt_class = proc->do_send_0(qt_imod, proc->mmobj()->mm_string_new(obj->metaObject()->className()));
    oop instance = proc->mmobj()->alloc_instance(qt_class);
    set_qt_instance(proc->mmobj(), instance, obj);
    set_meme_instance(obj, instance);
    return instance;
  }
}

static std::map<void*, oop> meme_mapping;

static
void set_meme_instance(QListWidgetItem* obj, oop instance) {
  meme_mapping[obj] = instance;
}

static
oop meme_instance(Process* proc, QListWidgetItem* obj) {
  if (meme_mapping.find(obj) == meme_mapping.end()) {
    oop qt_imod = proc->mp();
    oop qt_class = proc->do_send_0(qt_imod, proc->mmobj()->mm_string_new("QListWidgetItem"));
    oop instance = proc->mmobj()->alloc_instance(qt_class);
    set_qt_instance(proc->mmobj(), instance, obj);
    meme_mapping[obj] = instance;
    return instance;
  } else {
    return meme_mapping[obj];
  }
}

////////// bindings /////////


/** QApplication **/

static int prim_qapplication_new(Process* proc) {
  oop data_self =  proc->dp();
  QApplication* app = new QApplication(proc->vm()->argc(), proc->vm()->argv());
  set_meme_instance(app, proc->rp());
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
  oop instance = meme_instance(proc, app->focusWidget());
  proc->stack_push(instance);
  return 0;
}

/** QAction **/

static int prim_qaction_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_parent = *((oop*) proc->fp() - 1);
  oop oop_label = *((oop*) proc->fp() - 2);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QAction* qtobj = new QAction(proc->mmobj()->mm_string_cstr(oop_label), parent);
  set_meme_instance(qtobj, proc->rp());
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

  set_meme_instance(qtobj, proc->rp());
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


/** QEventLoop **/

static int prim_qt_qeventloop_new(Process* proc) {
  oop data_self =  proc->dp();

  QEventLoop* qtobj = new QEventLoop();

  set_meme_instance(qtobj, proc->rp());
  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qeventloop_exec(Process* proc) {
  oop data_self =  proc->dp();

  QEventLoop* qtobj = (QEventLoop*) get_qt_instance(proc->mmobj(), data_self);
  int val = qtobj->exec();
  proc->stack_push(tag_small_int(val));
  return 0;
}

static int prim_qt_qeventloop_exit(Process* proc) {
  oop data_self =  proc->dp();
  oop code = *((oop*) proc->fp() - 1);

  QEventLoop* qtobj = (QEventLoop*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->exit(untag_small_int(code));
  proc->stack_push(proc->rp());
  return 0;
}

/** QHBoxLayout **/

static int prim_qt_qhboxlayout_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_parent = *((oop*) proc->fp() - 1);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QHBoxLayout* qtobj = new QHBoxLayout(parent);

  set_meme_instance(qtobj, proc->rp());
  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}

// static int prim_qt_qhboxlayout_add_layout(Process* proc) {
//   oop data_self =  proc->dp();
//   oop oop_layout = *((oop*) proc->fp() - 1);

//   QLayout* layout = (QLayout*) get_qt_instance(proc->mmobj(), oop_layout);
//   QHBoxLayout* qtobj = (QHBoxLayout*) get_qt_instance(proc->mmobj(), data_self);
//   qtobj->addLayout(layout);
//   proc->stack_push(proc->rp());
//   return 0;
// }

static int prim_qt_qhboxlayout_add_widget(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_widget = *((oop*) proc->fp() - 1);

  QWidget* widget = (QWidget*) get_qt_instance(proc->mmobj(), oop_widget);
  QHBoxLayout* qtobj = (QHBoxLayout*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->addWidget(widget);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qhboxlayout_set_contents_margins(Process* proc) {
  oop data_self =  proc->dp();
  oop b = *((oop*) proc->fp() - 1);
  oop r = *((oop*) proc->fp() - 1);
  oop t = *((oop*) proc->fp() - 1);
  oop l = *((oop*) proc->fp() - 1);

  QHBoxLayout* qtobj = (QHBoxLayout*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->setContentsMargins(untag_small_int(l),
                            untag_small_int(t),
                            untag_small_int(r),
                            untag_small_int(b));
  proc->stack_push(proc->rp());
  return 0;
}

/** QHeaderView **/

static int prim_qt_qheaderview_hide(Process* proc) {
  oop data_self =  proc->dp();

  QHeaderView* qtobj = (QHeaderView*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->hide();
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_set_stretch_last_section(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_bool = *((oop*) proc->fp() - 1);

  QHeaderView* qtobj = (QHeaderView*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->setStretchLastSection(proc->mmobj()->mm_bool(oop_bool));
  proc->stack_push(proc->rp());
  return 0;
}


/** QLabel **/

static int prim_qt_qlabel_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_parent = *((oop*) proc->fp() - 1);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QLabel* qtobj = new QLabel(parent);

  set_meme_instance(qtobj, proc->rp());
  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qlabel_set_text(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_text = *((oop*) proc->fp() - 1);

  QLabel* qtobj = (QLabel*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->setText(proc->mmobj()->mm_string_cstr(oop_text));
  proc->stack_push(proc->rp());
  return 0;
}


/** QLayout **/

static int prim_qt_qlayout_add_widget(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_widget = *((oop*) proc->fp() - 1);

  QWidget* widget = (QWidget*) get_qt_instance(proc->mmobj(), oop_widget);
  QLayout* qtobj = (QLayout*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->addWidget(widget);
  proc->stack_push(proc->rp());
  return 0;
}


/** QLineEdit **/

static int prim_qt_qlineedit_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_parent = *((oop*) proc->fp() - 1);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QLineEdit* qtobj = new QLineEdit(parent);

  set_meme_instance(qtobj, proc->rp());
  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qlineedit_select_all(Process* proc) {
  oop data_self =  proc->dp();

  QLineEdit* qtobj = (QLineEdit*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->selectAll();
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qlineedit_selected_text(Process* proc) {
  oop data_self =  proc->dp();

  QLineEdit* qtobj = (QLineEdit*) get_qt_instance(proc->mmobj(), data_self);
  proc->stack_push(proc->mmobj()->mm_string_new(qtobj->selectedText().toLocal8Bit().data()));
  return 0;
}

static int prim_qt_qlineedit_set_text(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_text = *((oop*) proc->fp() - 1);

  QLineEdit* qtobj = (QLineEdit*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->setText(proc->mmobj()->mm_string_cstr(oop_text));
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qlineedit_text(Process* proc) {
  oop data_self =  proc->dp();

  QLineEdit* qtobj = (QLineEdit*) get_qt_instance(proc->mmobj(), data_self);
  proc->stack_push(proc->mmobj()->mm_string_new(qtobj->text().toLocal8Bit().data()));
  return 0;
}

static int prim_qt_qlineedit_set_selection(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_length = *((oop*) proc->fp() - 1);
  oop oop_start = *((oop*) proc->fp() - 2);

  QLineEdit* qtobj = (QLineEdit*) get_qt_instance(proc->mmobj(), data_self);
  qtobj->setSelection(untag_small_int(oop_start), untag_small_int(oop_length));
  proc->stack_push(proc->rp());
  return 0;
}


/** QListWidget **/

static int prim_qt_qlistwidget_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_parent = *((oop*) proc->fp() - 1);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QListWidget* qtobj = new QListWidget(parent);

  set_meme_instance(qtobj, proc->rp());
  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qlistwidget_current_item(Process* proc) {
  oop data_self =  proc->dp();

  QListWidget* qtobj = (QListWidget*) get_qt_instance(proc->mmobj(), data_self);
  proc->stack_push(meme_instance(proc, qtobj->currentItem()));
  return 0;
}

/** QListWidgetItem **/

static int prim_qt_qlistwidgetitem_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_text = *((oop*) proc->fp() - 1);
  oop oop_parent = *((oop*) proc->fp() - 2);

  QListWidget* parent = (QListWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QListWidgetItem* qtobj = new QListWidgetItem(proc->mmobj()->mm_string_cstr(oop_text), parent);

  set_meme_instance(qtobj, proc->rp());
  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qlistwidgetitem_text(Process* proc) {
  oop data_self =  proc->dp();

  QListWidgetItem* qtobj = (QListWidgetItem*) get_qt_instance(proc->mmobj(), data_self);
  proc->stack_push(proc->mmobj()->mm_string_new(qtobj->text().toLocal8Bit().data()));
  return 0;
}


/** QMainWindow **/

class _QMainWindow : public QMainWindow {
public:
  _QMainWindow(QWidget* parent, oop self, Process* proc) : QMainWindow(parent), _self(self), _proc(proc) {};
  void closeEvent(QCloseEvent* ev) {
    std::pair<oop, oop> r = _proc->lookup(_self, _proc->mmobj()->mm_object_vt(_self), _proc->vm()->new_symbol("closeEvent"));
    if (!r.first && !r.second) {
      QMainWindow::closeEvent(ev);
    } else {
      _proc->do_send_0(_self, _proc->vm()->new_symbol("closeEvent"));
    }
  };
private:
  oop _self;
  Process* _proc;
};

static int prim_qt_qmainwindow_new(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_parent = *((oop*) proc->fp() - 1);

  QWidget* parent = (QWidget*) get_qt_instance(proc->mmobj(), oop_parent);

  QMainWindow* qtobj = new _QMainWindow(parent, proc->rp(), proc);

  set_meme_instance(qtobj, proc->rp());
  set_qt_instance(proc->mmobj(), data_self, qtobj);
  proc->stack_push(proc->rp());
  return 0;
}


static int prim_qt_qmainwindow_menu_bar(Process* proc) {
  oop data_self =  proc->dp();

  _QMainWindow* qtobj = (_QMainWindow*) get_qt_instance(proc->mmobj(), data_self);
  proc->stack_push(meme_instance(proc, qtobj->menuBar()));
  return 0;
}

static int prim_qt_qmainwindow_set_central_widget(Process* proc) {
  oop data_self =  proc->dp();
  oop oop_widget = *((oop*) proc->fp() - 1);

  QWidget* widget = (QWidget*) get_qt_instance(proc->mmobj(), oop_widget);
  _QMainWindow* qtobj = (_QMainWindow*) get_qt_instance(proc->mmobj(), data_self);

  qtobj->setCentralWidget(widget);
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qt_qmainwindow_status_bar(Process* proc) {
  oop data_self =  proc->dp();

  _QMainWindow* qtobj = (_QMainWindow*) get_qt_instance(proc->mmobj(), data_self);
  proc->stack_push(meme_instance(proc, qtobj->statusBar()));
  return 0;
}


/** QMenuBar **/
static int prim_qt_qmenubar_add_menu(Process* proc) {
  oop data_self =  proc->dp();
  oop text = *((oop*) proc->fp() - 1);

  QMenuBar* qtobj = (QMenuBar*) get_qt_instance(proc->mmobj(), data_self);

  qtobj->addMenu(proc->mmobj()->mm_string_cstr(text));
  proc->stack_push(proc->rp());
  return 0;
}


////////////


static int prim_qwidget_new(Process* proc) {
  oop data_self =  proc->dp();
  QWidget* qtobj = new QWidget;
  // debug() << "QT: qwidget_new rp: " << proc->rp() << " dp: " << data_self << " widget: " << w << endl;
  set_meme_instance(qtobj, proc->rp());
  set_qt_instance(proc->mmobj(), data_self, qtobj);
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

  vm->register_primitive("qt_qeventloop_new", prim_qt_qeventloop_new);
  vm->register_primitive("qt_qeventloop_exec", prim_qt_qeventloop_exec);
  vm->register_primitive("qt_qeventloop_exit", prim_qt_qeventloop_exit);

  vm->register_primitive("qt_qhboxlayout_new", prim_qt_qhboxlayout_new);
  // vm->register_primitive("qt_qhboxlayout_add_layout", prim_qt_qhboxlayout_add_layout);
  vm->register_primitive("qt_qhboxlayout_add_widget", prim_qt_qhboxlayout_add_widget);
  vm->register_primitive("qt_qhboxlayout_set_contents_margins", prim_qt_qhboxlayout_set_contents_margins);

  vm->register_primitive("qt_qheaderview_hide", prim_qt_qheaderview_hide);
  vm->register_primitive("qt_qheaderview_set_stretch_last_section", prim_qt_set_stretch_last_section);

  vm->register_primitive("qt_qlabel_new", prim_qt_qlabel_new);
  vm->register_primitive("qt_qlabel_set_text", prim_qt_qlabel_set_text);

  vm->register_primitive("qt_qlayout_add_widget", prim_qt_qlayout_add_widget);

  vm->register_primitive("qt_qlineedit_new", prim_qt_qlineedit_new);
  vm->register_primitive("qt_qlineedit_select_all", prim_qt_qlineedit_select_all);
  vm->register_primitive("qt_qlineedit_selected_text", prim_qt_qlineedit_selected_text);
  vm->register_primitive("qt_qlineedit_set_selection", prim_qt_qlineedit_set_selection);
  vm->register_primitive("qt_qlineedit_set_text", prim_qt_qlineedit_set_text);
  vm->register_primitive("qt_qlineedit_text", prim_qt_qlineedit_text);

  vm->register_primitive("qt_qlistwidget_new", prim_qt_qlistwidget_new);
  vm->register_primitive("qt_qlistwidget_current_item", prim_qt_qlistwidget_current_item);

  vm->register_primitive("qt_qlistwidgetitem_new", prim_qt_qlistwidgetitem_new);
  vm->register_primitive("qt_qlistwidgetitem_text", prim_qt_qlistwidgetitem_text);

  vm->register_primitive("qt_qmainwindow_new", prim_qt_qmainwindow_new);
  vm->register_primitive("qt_qmainwindow_menu_bar", prim_qt_qmainwindow_menu_bar);
  vm->register_primitive("qt_qmainwindow_set_central_widget", prim_qt_qmainwindow_set_central_widget);
  vm->register_primitive("qt_qmainwindow_status_bar", prim_qt_qmainwindow_status_bar);

  vm->register_primitive("qt_qmenubar_add_menu", prim_qt_qmenubar_add_menu);


  //


  // vm->register_primitive("qt_qplaintextedit_new", prim_qt_qplaintextedit_new);
  // vm->register_primitive("qt_qplaintextedit_set_plain_text", prim_qt_qplaintextedit_set_plain_text);
  // vm->register_primitive("qt_qplaintextedit_set_tabstop_width", prim_qt_qplaintextedit_set_tabstop_width);
  // vm->register_primitive("qt_qplaintextedit_set_text_cursor", prim_qt_qplaintextedit_set_text_cursor);
  // vm->register_primitive("qt_qplaintextedit_to_plain_text", prim_qt_qplaintextedit_to_plain_text);

  // vm->register_primitive("qt_scintilla_editor_new", prim_qt_scintilla_editor_new);
  // vm->register_primitive("qt_scintilla_append", prim_qt_scintilla_append);
  // vm->register_primitive("qt_scintilla_copy", prim_qt_scintilla_copy);
  // vm->register_primitive("qt_scintilla_cut", prim_qt_scintilla_cut);
  // vm->register_primitive("qt_scintilla_get_cursor_position", prim_qt_scintilla_get_cursor_position);
  // vm->register_primitive("qt_scintilla_get_selection", prim_qt_scintilla_get_selection);
  // vm->register_primitive("qt_scintilla_insert_at", prim_qt_scintilla_insert_at);
  // vm->register_primitive("qt_scintilla_lines", prim_qt_scintilla_lines);
  // vm->register_primitive("qt_scintilla_paste", prim_qt_scintilla_paste);
  // vm->register_primitive("qt_scintilla_paused_at_line", prim_qt_scintilla_paused_at_line);
  // vm->register_primitive("qt_scintilla_redo", prim_qt_scintilla_redo);
  // vm->register_primitive("qt_scintilla_saved", prim_qt_scintilla_saved);
  // vm->register_primitive("qt_scintilla_selected_text", prim_qt_scintilla_selected_text);
  // vm->register_primitive("qt_scintilla_set_selection", prim_qt_scintilla_set_selection);
  // vm->register_primitive("qt_scintilla_set_text", prim_qt_scintilla_set_text);
  // vm->register_primitive("qt_scintilla_text", prim_qt_scintilla_text);
  // vm->register_primitive("qt_scintilla_undo", prim_qt_scintilla_undo);

  // vm->register_primitive("qt_qshortcut_new", prim_qt_qshortcut_new);
  // vm->register_primitive("qt_qshortcut_set_context", prim_qt_qshortcut_set_context);

  // vm->register_primitive("qt_qtablewidget_new", prim_qt_qtablewidget_new);
  // vm->register_primitive("qt_qtablewidget_clear", prim_qt_qtablewidget_clear);
  // vm->register_primitive("qt_qtablewidget_horizontal_header", prim_qt_qtablewidget_horizontal_header);
  // vm->register_primitive("qt_qtablewidget_set_column_count", prim_qt_qtablewidget_set_column_count);
  // vm->register_primitive("qt_qtablewidget_set_horizontal_header_labels", prim_qt_qtablewidget_set_horizontal_header_labels);
  // vm->register_primitive("qt_qtablewidget_set_item", prim_qt_qtablewidget_set_item);
  // vm->register_primitive("qt_qtablewidget_set_row_count", prim_qt_qtablewidget_set_row_count);
  // vm->register_primitive("qt_qtablewidget_set_selection_mode", prim_qt_qtablewidget_set_selection_mode);
  // vm->register_primitive("qt_qtablewidget_set_sorting_enabled", prim_qt_qtablewidget_set_sorting_enabled);
  // vm->register_primitive("qt_qtablewidget_vertical_header", prim_qt_qtablewidget_vertical_header);
  // vm->register_primitive("qt_qtablewidgetitem_new", prim_qt_qtablewidgetitem_new);
  // vm->register_primitive("qt_qtablewidgetitem_set_flags", prim_qt_qtablewidgetitem_set_flags);

  // vm->register_primitive("qt_qtextcursor_drag_right", prim_qt_qtextcursor_drag_right);
  // vm->register_primitive("qt_qtextcursor_insert_text", prim_qt_qtextcursor_insert_text);
  // vm->register_primitive("qt_qtextcursor_selected_text", prim_qt_qtextcursor_selected_text);
  // vm->register_primitive("qt_qtextcursor_selection_end", prim_qt_qtextcursor_selection_end);
  // vm->register_primitive("qt_qtextcursor_set_position", prim_qt_qtextcursor_set_position);

  // vm->register_primitive("qt_qurl_fragment", prim_qt_qurl_fragment);
  // vm->register_primitive("qt_qurl_has_fragment", prim_qt_qurl_has_fragment);
  // vm->register_primitive("qt_qurl_has_query_item", prim_qt_qurl_has_query_item);
  // vm->register_primitive("qt_qurl_path", prim_qt_qurl_path);
  // vm->register_primitive("qt_qurl_query_item_value", prim_qt_qurl_query_item_value);
  // vm->register_primitive("qt_qurl_to_string", prim_qt_qurl_to_string);

  // vm->register_primitive("qt_qvboxlayout_new", prim_qt_qvboxlayout_new);
  // vm->register_primitive("qt_qvboxlayout_add_layout", prim_qt_qvboxlayout_add_layout);
  // vm->register_primitive("qt_qvboxlayout_add_widget", prim_qt_qvboxlayout_add_widget);

  // vm->register_primitive("qt_qwebelement_append_inside", prim_qt_qwebelement_append_inside);
  // vm->register_primitive("qt_qwebelement_append_outside", prim_qt_qwebelement_append_outside);
  // vm->register_primitive("qt_qwebelement_clone", prim_qt_qwebelement_clone);
  // vm->register_primitive("qt_qwebelement_find_first", prim_qt_qwebelement_find_first);
  // vm->register_primitive("qt_qwebelement_set_attribute", prim_qt_qwebelement_set_attribute);
  // vm->register_primitive("qt_qwebelement_set_inner_xml", prim_qt_qwebelement_set_inner_xml);
  // vm->register_primitive("qt_qwebelement_set_plain_text", prim_qt_qwebelement_set_plain_text);
  // vm->register_primitive("qt_qwebelement_set_style_property", prim_qt_qwebelement_set_style_property);
  // vm->register_primitive("qt_qwebelement_take_from_document", prim_qt_qwebelement_take_from_document);
  // vm->register_primitive("qt_qwebelement_to_outer_xml", prim_qt_qwebelement_to_outer_xml);

  // vm->register_primitive("qt_qwebframe_add_to_javascript_window_object", prim_qt_qwebframe_add_to_javascript_window_object);
  // vm->register_primitive("qt_qwebframe_document_element", prim_qt_qwebframe_document_element);
  // vm->register_primitive("qt_qwebframe_scroll_to_anchor", prim_qt_qwebframe_scroll_to_anchor);

  // vm->register_primitive("qt_extra_qwebpage_enable_plugins", prim_qt_extra_qwebpage_enable_plugins);

  // vm->register_primitive("qt_qwebpage_main_frame", prim_qt_qwebpage_main_frame);
  // vm->register_primitive("qt_qwebpage_set_link_delegation_policy", prim_qt_qwebpage_set_link_delegation_policy);

  // vm->register_primitive("qt_qwebview_new", prim_qt_qwebview_new);
  // vm->register_primitive("qt_qwebview_page", prim_qt_qwebview_page);
  // vm->register_primitive("qt_qwebview_set_html", prim_qt_qwebview_set_html);
  // vm->register_primitive("qt_qwebview_set_url", prim_qt_qwebview_set_url);

  // vm->register_primitive("qt_qwidget_new", prim_qt_qwidget_new);
  // vm->register_primitive("qt_qwidget_actions", prim_qt_qwidget_actions);
  // vm->register_primitive("qt_qwidget_add_action", prim_qt_qwidget_add_action);
  // vm->register_primitive("qt_qwidget_close", prim_qt_qwidget_close);
  // vm->register_primitive("qt_qwidget_connect", prim_qt_qwidget_connect);
  // vm->register_primitive("qt_qwidget_has_focus", prim_qt_qwidget_has_focus);
  // vm->register_primitive("qt_qwidget_hide", prim_qt_qwidget_hide);
  // vm->register_primitive("qt_qwidget_is_visible", prim_qt_qwidget_is_visible);
  // vm->register_primitive("qt_qwidget_resize", prim_qt_qwidget_resize);
  // vm->register_primitive("qt_qwidget_set_focus", prim_qt_qwidget_set_focus);
  // vm->register_primitive("qt_qwidget_set_maximum_height", prim_qt_qwidget_set_maximum_height);
  // vm->register_primitive("qt_qwidget_set_maximum_width", prim_qt_qwidget_set_maximum_width);
  // vm->register_primitive("qt_qwidget_set_minimum_size", prim_qt_qwidget_set_minimum_size);
  // vm->register_primitive("qt_qwidget_set_minimum_width", prim_qt_qwidget_set_minimum_width);
  // vm->register_primitive("qt_qwidget_set_stylesheet", prim_qt_qwidget_set_stylesheet);
  // vm->register_primitive("qt_qwidget_set_window_title", prim_qt_qwidget_set_window_title);
  // vm->register_primitive("qt_qwidget_show", prim_qt_qwidget_show);


  ///

  vm->register_primitive("qt_qwidget_new", prim_qwidget_new);
  vm->register_primitive("qt_qwidget_show", prim_qwidget_show);
  vm->register_primitive("qt_qwidget_connect", prim_qwidget_connect);

  vm->register_primitive("qt_qpushbutton_new", prim_qpushbutton_new);
}
