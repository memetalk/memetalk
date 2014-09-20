.preamble(qt, io)
  qt : meme:qt;
  io : meme:io;
  [QWidget, QMainWindow, QsciScintilla, QLineEdit, QComboBox, QTableWidget, QListWidgetItem, QTableWidgetItem] <= qt;
.code

class ExecutionFrames
fields: proc;
init new: fun(proc) {
  @proc = proc;
}

instance_method codeFor: fun(i) {
  var cp = @proc.frames()[i].cp().compiledFunction();
  if (cp.isTopLevel()) {
    return cp.text();
  } else {
    return cp.outerCompiledFunction().text();
  }
// else {
//     if (cp.isEmbedded()) {
//       return cp.topLevelCompiledFunction().text();
//     } else {
//       return cp.text();
//     }
//   }
}

instance_method frame: fun(i) {
  return @proc.frames()[i];
}

instance_method locationInfoFor: fun(i) {
  var ip = @proc.frames()[i].ip();
  return @proc.frames()[i].cp().compiledFunction().source_location_for_ip(ip);
}

instance_method names: fun() {
  return @proc.frames().map(fun(frame) {
    frame.cp.compiledFunction.fullName + ":" + (frame.cp.compiledFunction.lineForInstruction(frame.ip) + 1).toString
  });
}

instance_method size: fun() {
  return @proc.frames().size();
}

instance_method topFrame: fun() {
  return @proc.frames()[this.size()-1];
}

end //idez:ExecutionFrames

class StackCombo < QComboBox
fields: frames, on_update;
init new: fun(parent, execframes) {
  super.new(parent);
  @frames = execframes;
  @on_update = fun(_) { };
  // @updating = false;

  this.connect("currentIndexChanged(int)", fun(idx) {
    // if (!@updating) {
      @on_update(idx);
    // }
  });
}

instance_method onUpdate: fun(fn) {
  @on_update = fn;
}
instance_method updateInfo: fun() {
  this.clear();
  @frames.names().each(fun(name) {
    this.addItem(name);
  });
  this.setCurrentIndex(0);
}

end

class Ddd < QMainWindow
fields: process, editor, stackCombo, execFrames, frame_index, localVarList, fieldVarList;
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
  action.setShortcut("ctrl+k");
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

  action = qt.QAction.new("Do it", this);
  action.setShortcut("ctrl+d");
  action.connect("triggered", fun(_) {
      this.doIt();
  });
  action.setShortcutContext(0); //widget context
  execMenu.addAction(action);

  action = qt.QAction.new("Print it", this);
  action.setShortcut("ctrl+p");
  action.connect("triggered", fun(_) {
      this.printIt();
  });
  action.setShortcutContext(0); //widget context
  execMenu.addAction(action);

  action = qt.QAction.new("Reload frame", this);
  action.setShortcut("ctrl+r");
  action.connect("triggered", fun(_) {
      this.reloadFrame();
  });
  action.setShortcutContext(0); //widget context
  execMenu.addAction(action);

  var centralWidget = QWidget.new(this);
  this.setCentralWidget(centralWidget);
  var mainLayout = qt.QVBoxLayout.new(centralWidget);
  @editor = QsciScintilla.new(centralWidget);
  mainLayout.addWidget(@editor);
  // @editor.setText(@process.cp().compiledFunction().text());

  @frame_index = 0;
  @execFrames = ExecutionFrames.new(@process);
  @stackCombo = StackCombo.new(centralWidget, @execFrames);
  @stackCombo.onUpdate(fun(i) {
    @frame_index = i;
    this.updateUI();
  });

  mainLayout.addWidget(@stackCombo);

  var hbox = qt.QHBoxLayout.new(null);
  @localVarList = VariableListWidget.new(@process, centralWidget);
  hbox.addWidget(@localVarList);

  @fieldVarList = VariableListWidget.new(@process, centralWidget);
  hbox.addWidget(@fieldVarList);
  mainLayout.addLayout(hbox);
}

instance_method process_paused: fun() { //this is called from the vm
  @stackCombo.updateInfo();
  //this.updateUI();
}

instance_method insertSelectedText: fun(text) {
  var pos = @editor.getSelection();
  @editor.insertAt(text, pos[:end_line], pos[:end_index]);
  var nl = text.count("\n");
  if (nl == 0) {
    @editor.setSelection(pos[:end_line], pos[:end_index], pos[:end_line], pos[:end_index] + text.size);
  } else {
    var pl = text.rindex("\n");
    @editor.setSelection(pos[:end_line], pos[:end_index], pos[:end_line] + nl, text.from(pl).size() - 1);
  }
}

instance_method doIt: fun() {
  try {
    var ctx = Context.withFrame(@editor.selectedText(), @process.frames()[@frame_index], thisModule);
    //@process.apply(ctx);
    ctx();
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method printIt: fun() {
  try {
    var ctx = Context.withFrame(@editor.selectedText(), @process.frames()[@frame_index], thisModule);
    // var res = @process.apply(ctx);
    var res = ctx();
    this.insertSelectedText(res.toString);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method reloadFrame: fun() {
  @process.reloadFrame();
  this.updateUI;
}

instance_method updateUI: fun() {
  if (@frame_index >= 0) {
    @editor.setText(@execFrames.codeFor(@frame_index));
    // @editor.setFrameIndex(@frame_index);
    var locInfo = @execFrames.locationInfoFor(@frame_index);
    if (locInfo) {
      @editor.pausedAtLine(locInfo[0], locInfo[1], locInfo[2], locInfo[3]);
    }
    @localVarList.loadFrame(@process.frames[@frame_index]);
    @fieldVarList.loadReceiver(@process.frames[@frame_index]);
  }

  // @editor.setText(@process.cp().compiledFunction().text());
  // var locInfo = @process.cp().compiledFunction().source_location_for_ip(@process.ip);
  // if (locInfo) {
  //   @editor.pausedAtLine(locInfo[0], locInfo[1], locInfo[2], locInfo[3]);
  // }
  // //vars
}
end


class VariableItem < QTableWidgetItem
fields: obj;
init new: fun(text, obj) {
  super.new(text);
  this.setFlags(33);
  @obj = obj;
}

instance_method object: fun() {
  return @obj;
}

end //idez:VariableItem

class VariableListWidget < QTableWidget
fields: target_process;
init new: fun(process, parent) {
  super.new(parent);
  this.verticalHeader().hide();
  this.setSelectionMode(1);
  var header = this.horizontalHeader();
  header.setStretchLastSection(true);
  this.setSortingEnabled(false);
  this.setColumnCount(2);

  this.clear();
  this.setHorizontalHeaderLabels(['Name', 'Value']);

  @target_process = process;

  this.connect("itemDoubleClicked", fun(item) {
    io.print("Inspector.inspect!")
    //Inspector.inspect(item.object)
  });
}

instance_method loadFrame: fun(frame) {
  this.clear();
  this.setHorizontalHeaderLabels(['Name', 'Value']);

  var variables = frame.cp.compiledFunction.env_table;
  this.setRowCount(variables.size + 2);

  var _this = frame.rp;

  this.setItem(0, 0, VariableItem.new("this", _this));
  this.setItem(0, 1, VariableItem.new(_this.toString, _this));

  var _dthis = frame.dp;
  this.setItem(1, 0, VariableItem.new("@this", _dthis));
  this.setItem(1, 1, VariableItem.new(_dthis.toString, _dthis));

  variables.each(fun(idx, varname) {
    var entry = frame.get_local_value(idx);
    this.setItem(idx + 2, 0, VariableItem.new(varname.toString, entry));
    this.setItem(idx + 2, 1, VariableItem.new(entry.toString, entry));
  });
}
//end idez:VariableListWidget:loadFrame

instance_method loadReceiver: fun(frame) {
  io.print("load receiver!!");
  return 1;

  this.clear();
  this.setHorizontalHeaderLabels(['Name', 'Value']);

  var _dthis = frame.dp;
  var mirror = Mirror.new(_dthis);
  var fields = mirror.fields();
  this.setRowCount(fields.size);

  fields.each(fun(i, name) {
    this.setItem(i, 0, VariableItem.new(name, mirror.valueFor(name)));
    this.setItem(i, 1, VariableItem.new(mirror.valueFor(name).toString, mirror.valueFor(name)));
  });
}

end //idez:VariableListWidget


main: fun(proc) {
  var app = qt.QApplication.new();
  var d = Ddd.new(proc);
  d.show();
  return d;
}
.endcode
