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
  io.print("codeFor: " + i.toString);
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
  io.print("locationInfofor: " + i.toString);
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
  io.print("topFrame:" + (this.size()-1).toString);
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
    io.print("StackCombo: currentIndexChanged slot");
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
fields: process, editor, stackCombo, execFrames, frame_index;
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

  var centralWidget = QWidget.new(this);
  this.setCentralWidget(centralWidget);
  var mainLayout = qt.QVBoxLayout.new(centralWidget);
  @editor = QsciScintilla.new(centralWidget);
  mainLayout.addWidget(@editor);
  @editor.setText(@process.cp().compiledFunction().text());

  @frame_index = 0;
  @execFrames = ExecutionFrames.new(@process);
  @stackCombo = StackCombo.new(centralWidget, @execFrames);
  mainLayout.addWidget(@stackCombo);

  @stackCombo.onUpdate(fun(i) {
    io.print("stackCombo: onUpdate: " + i.toString);
    @frame_index = i;
    this.updateUI();
  });
}

instance_method process_paused: fun() { //this is called from the vm
  io.print("DDD: process_paused");
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
    io.print("Eval in frame: " + @frame_index.toString + " -- " + @editor.selectedText());
    var ctx = Context.withFrame(@editor.selectedText(), @process.frames()[@frame_index], thisModule);
    @process.apply(ctx);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method printIt: fun() {
  try {
    io.print("Eval in frame: " + @frame_index.toString + " -- " + @editor.selectedText());
    var ctx = Context.withFrame(@editor.selectedText(), @process.frames()[@frame_index], thisModule);
    var res = @process.apply(ctx);
    this.insertSelectedText(res.toString);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method updateUI: fun() {
  io.print("DDD: updateUI " + @frame_index.toString);
  if (@frame_index >= 0) {
    @editor.setText(@execFrames.codeFor(@frame_index));
    // @editor.setFrameIndex(@frame_index);
    var locInfo = @execFrames.locationInfoFor(@frame_index);
    if (locInfo) {
      io.print(locInfo);
      @editor.pausedAtLine(locInfo[0], locInfo[1], locInfo[2], locInfo[3]);
    }
  }
  // @editor.setText(@process.cp().compiledFunction().text());
  // var locInfo = @process.cp().compiledFunction().source_location_for_ip(@process.ip);
  // if (locInfo) {
  //   @editor.pausedAtLine(locInfo[0], locInfo[1], locInfo[2], locInfo[3]);
  // }
  // //vars
}
end

main: fun(proc) {
  var app = qt.QApplication.new();
  var d = Ddd.new(proc);
  d.show();
  return d;
}
.endcode
