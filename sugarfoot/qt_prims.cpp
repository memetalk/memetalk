#include "qt_prims.hpp"
#include "vm.hpp"
#include "process.hpp"
#include "report.hpp"
#include <QApplication>
#include <QWidget>

static int prim_qapplication_new(Process* proc) {
  oop self =  proc->dp();
  QApplication* app = new QApplication(proc->vm()->argc(), proc->vm()->argv());
  debug() << "QApplication " << app << endl;
  ((oop*)self)[2] = (oop) app;
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qapplication_exec(Process* proc) {
  oop self =  proc->dp();
  QApplication* app = (QApplication*) ((oop*)self)[2];
  debug() << "QApplication:exec " << app << endl;
  app->exec();
  proc->stack_push(proc->dp());
  return 0;
}

static int prim_qwidget_new(Process* proc) {
  oop self =  proc->dp();
  QWidget* w = new QWidget;
  debug() << "QWidget " << w << endl;
  ((oop*)self)[2] = (oop) w;
  proc->stack_push(proc->rp());
  return 0;
}

static int prim_qwidget_show(Process* proc) {
  oop self =  proc->dp();
  QWidget* w = (QWidget*) ((oop*)self)[2];
  debug() << "QWidget " << w << endl;
  w->show();
  proc->stack_push(proc->rp());
  return 0;
}

void qt_init_primitives(VM* vm) {
  vm->register_primitive("qt_qapplication_new", prim_qapplication_new);
  vm->register_primitive("qt_qapplication_exec", prim_qapplication_exec);

  vm->register_primitive("qt_qwidget_new", prim_qwidget_new);
  vm->register_primitive("qt_qwidget_show", prim_qwidget_show);
}
