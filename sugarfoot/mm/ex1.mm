.preamble(qt, io)
 qt : meme:qt;
 io : meme:io;
 [QMainWindow, QWidget] <= qt;
.code

class Debugger < QMainWindow
fields: process;
init new: fun(proc) {
  super.new(null);
  @process = proc;
  this.resize(200,200);
  var action = qt.QAction.new("Step", this);
  action.setShortcut("ctrl+s");
  action.connect("triggered", fun(_) {
      this.step();
  });
  action.setShortcutContext(0); //widget context
  var execMenu = this.menuBar().addMenu("Exec");
  execMenu.addAction(action);
}

instance_method step: fun() {
  @process.step();
}
end

maybe_init_qt: fun() {
  <primitive "qt_maybe_construct_qapp">
}

dbg_main: fun(proc) {
  io.print("dbg_main: maybe init qt");
  maybe_init_qt();
  io.print("dbg_main: inited");
  var d = Debugger.new(proc);
  io.print("dbg_main: dbg new");
  d.show();
  io.print("dbg_main: dbg shown");
}

debug: fun() {
  <primitive "test_debug">
}

bar: fun() {
  io.print("bar: caling debug");
  debug();
  io.print("bar: debug returned");
}

f: fun() {
  <primitive "test_catch_exception">
}

main: fun() {
  io.print("main: caling bar");
  //bar();
  f();
  io.print("main: bar returned");
}

.endcode
