.preamble(qt, io)
  qt : meme:qt;
  io : meme:io;
  [QWidget, QMainWindow, QsciScintilla, QLineEdit, QComboBox, QTableWidget, QListWidgetItem, QTableWidgetItem] <= qt;
.code

class Ddd < QMainWindow
fields: process, editor;
init new: fun(proc) {
  super.new(null);
  this.setWindowTitle("Debugger");

  @process = proc;
  this.resize(600,300);

  var execMenu = this.menuBar().addMenu("Exec");

  var action = qt.QAction.new("Step into", this);
  action.setShortcut("ctrl+s");
  action.connect("triggered", fun(_) {
    @process.stepInto();
  });
  action.setShortcutContext(0); //widget context
  execMenu.addAction(action);

  action = qt.QAction.new("Step over Line", this);
  action.setShortcut("ctrl+p");
  action.connect("triggered", fun(_) {
      @process.stepOverLine();
  });
  action.setShortcutContext(0); //widget context
  execMenu.addAction(action);

  action = qt.QAction.new("Step over", this);
  action.setShortcut("ctrl+o");
  action.connect("triggered", fun(_) {
      @process.stepOver();
  });
  action.setShortcutContext(0); //widget context
  execMenu.addAction(action);

  action = qt.QAction.new("Step out", this);
  action.setShortcut("ctrl+u");
  action.connect("triggered", fun(_) {
      @process.stepOut();
  });
  action.setShortcutContext(0); //widget context
  execMenu.addAction(action);

  var centralWidget = QWidget.new(this);
  this.setCentralWidget(centralWidget);
  var mainLayout = qt.QVBoxLayout.new(centralWidget);
  @editor = QsciScintilla.new(centralWidget);
  mainLayout.addWidget(@editor);
  @editor.setText(@process.cp().compiledFunction().text());
}

instance_method process_paused: fun() { //this is called from the vm
  io.print("DDD: PROCESS_PAUSED");
  @editor.setText(@process.cp().compiledFunction().text());
  var locInfo = @process.cp().compiledFunction().source_location_for_ip(@process.ip);
  if (locInfo) {
    @editor.pausedAtLine(locInfo[0], locInfo[1], locInfo[2], locInfo[3]);
  }
}
end

main: fun(proc) {
  var app = qt.QApplication.new();
  var d = Ddd.new(proc);
  d.show();
  return d;
}
.endcode
