module debugger(QWidget, QComboBox, QTableWidget, QMainWindow, qt) {

  // features:
  // -step over/into/out + continue + breakAt
  // -doIt/printIt/debugIt
  //
  // -setting/removing breakpoints
  //    -[ctrl+b] for list window of breakpoints to delete
  //
  // -editing variables sets them
  //   -evaluation of new value in the current frame.
  //   -we have the handle of the object by stack.frame().var("name")
  //   -we can set it with frame().var("name").become(newval)
  //
  // -change the stack combo, changes the current frame
  //    -updates the editor with the frame's context source and arrow location
  //    -updates variables
  //
  // -change source code + accept [ctrl+s] recompiled the function
  //   -same routine as in doIt:
  //      n_cfun = CompiledFunction.new(text, params, cmodule)
  //    then:
  //      fn = Function.new(n_cfun, stack.frame().imodule())
  //    and change it:
  //      stack.frame().ctx().become(fn)
  //    ** we will need to change CFun.new() so it does not receive a params
  //    ** it should be inferred from the parsing. doIt should create the text
  //    ** fun() { <selection< } instead of just { <selection> }
  //
  // Other features:
  //  -alt+s: dumps the system into .mm source codes
  //          -> for each registered module in the Interpreter:
  //               module.dump()
  //          -> I think it will be less painful to do it in memetalk:
  //             create a module, add to it some primitive funs bound
  //             to io functions and dump the source.
  //

  fun qapp_running() {
    <primitive "qapp_running">
  }

  fun print(arg) {
    <primitive "print">
  }

  // fun ast_line(arg) {
  //   <primitive "ast_line">
  // }

  class ScintillaEditor < QWidget {
    init new(parent) {
        <primitive "qt_scintilla_editor_new">
    }
    fun setText(text) {
        <primitive "qt_scintilla_editor_set_text">
    }
    fun pausedAtLine(line) {
        <primitive "qt_scintilla_paused_at_line">
    }
  }

  class StackCombo < QComboBox {
    fields: frames;
    init new(parent, execframes) {
      super.new(parent);
      @frames = execframes;
    }
    // fun initialize() {
    //   this.setCurrentIndex(@frames.size() - 1);
    // }
    fun updateInfo() {
      this.clear();
      @frames.names().each(fun(name) {
        this.addItem(name);
      });
      this.setCurrentIndex(@frames.size() - 1);
    }
  }

  class ExecutionFrames {
    fields: vmproc;
    init new(vmproc) {
      @vmproc = vmproc;
    }
    fun names() {
      return @vmproc.stackFrames().map(fun(frame) {
        frame.contextPointer().compiledFunction().name()
      }) + [@vmproc.contextPointer().compiledFunction().name()];
    }
    fun codeFor(i) {
      if (i < @vmproc.stackFrames().size()) {
        return @vmproc.stackFrames().get(i).contextPointer().compiledFunction().text();
      } else {
        return @vmproc.contextPointer().compiledFunction().text();
      }
    }
    fun currentLineFor(i) {
      if (i < @vmproc.stackFrames().size()) {
        return @vmproc.stackFrames().get(i).instructionPointer();
      } else {
        return @vmproc.instructionPointer();
      }
    }
    fun size() {
      return @vmproc.stackFrames().size()+1; //1: current "frame" in proc
    }
  }

  class VariableListWidget < QTableWidget {
    init new(parent) {
      super.new(2, 2, parent);
      this.setHorizontalHeaderLabels(['Name', 'Value']);
      this.verticalHeader().hide();
      this.setSelectionMode(1);
      var header = this.horizontalHeader();
      header.setStretchLastSection(true);

      var n1 = qt.QTableWidgetItem.new("field 1");
      var n2 = qt.QTableWidgetItem.new("field 2");

      var v1 = qt.QTableWidgetItem.new("val 1");
      var v2 = qt.QTableWidgetItem.new("val 2");

      this.setItem(0, 0, n1);
      this.setItem(1, 0, n2);
      this.setItem(0, 1, v1);
      this.setItem(1, 1, v2);
      this.setSortingEnabled(true);
    }
  }

  class DebuggerUI < QMainWindow {
    fields: eventloop, process, execFrames, stackCombo, editor, localsList, attrsList, scopedList;
    init new(eventloop, process) {
      super.new();
      @eventloop = eventloop;
      @process = process;

      this.resize(700,250);
      this.setWindowTitle("Debugger");
      var centralWidget = QWidget.new(this);
      var mainLayout = qt.QVBoxLayout.new(centralWidget);

      @execFrames = ExecutionFrames.new(process);

      @stackCombo = StackCombo.new(centralWidget, @execFrames);
      mainLayout.addWidget(@stackCombo);

      @editor = ScintillaEditor.new(centralWidget);
      mainLayout.addWidget(@editor);

      @stackCombo.connect("currentIndexChanged",fun(i) {
        if (0 <= i) {
          @editor.setText(@execFrames.codeFor(i));
          @editor.pausedAtLine(@execFrames.currentLineFor(i));
        }
      });
      @stackCombo.updateInfo();

      var hbox = qt.QHBoxLayout.new(null);
      @localsList = VariableListWidget.new(centralWidget);
      hbox.addWidget(@localsList);

      @attrsList = VariableListWidget.new(centralWidget);
      hbox.addWidget(@attrsList);

      @scopedList = VariableListWidget.new(centralWidget);
      hbox.addWidget(@scopedList);

      mainLayout.addLayout(hbox);
      this.setCentralWidget(centralWidget);

      var execMenu = this.menuBar().addMenu("&Debugging");
      var action = qt.QAction.new("Step &Into", execMenu);
      action.setShortcut("F6");
      action.connect("triggered", fun() {
        this.stepInto()
      });
      execMenu.addAction(action);

      var action = qt.QAction.new("Step &Over", execMenu);
      action.setShortcut("F7");
      action.connect("triggered", fun() {
        this.stepOver()
      });
      execMenu.addAction(action);

      var action = qt.QAction.new("&Continue", execMenu);
      action.setShortcut("F5");
      action.connect("triggered", fun() {
        this.continue()
      });
      execMenu.addAction(action);
    }
    fun stepInto() {
      @process.stepInto();
      @stackCombo.updateInfo();
    }
    fun stepOver() {
      @process.stepOver();
      @stackCombo.updateInfo();
    }
    fun continue() {
      @process.continue();
      @stackCombo.updateInfo();
    }
  }

  fun main(process) {
    var eventloop = null;
    if (!qapp_running()) {
      eventloop = qt.QApplication.new();
    } else {
      eventloop = qt.QEventLoop.new();
    }

    var dbg = DebuggerUI.new(eventloop, process);
    dbg.show();
    eventloop.exec();
  }
}
