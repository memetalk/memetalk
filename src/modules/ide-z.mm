module idez(qt, QWidget, QMainWindow, QPlainTextEdit, QComboBox, QTableWidget) {


  fun getClass_CompiledFunction() {
    <primitive "class_compiled_function">
  }

  fun get_current_compiled_module() {
    <primitive "get_current_compiled_module">
  }

  fun get_mirror_class() {
    <primitive "get_mirror_class">
  }

  fun print(arg) {
    <primitive "print">
  }

  fun qapp_running() {
    <primitive "qapp_running">
  }

  fun switch_back() {
    <primitive "switch_back">
  }

  class Editor < QPlainTextEdit {
    fields: variables;
    init new(parent) {
      super.new(parent);
      @variables = {};

      this.setTabStopWidth(20);

      var action = qt.QAction.new("&Do it", this);
      action.setShortcut("ctrl+d");
      action.connect("triggered", fun() {
          this.doIt();
          switch_back();
      });
      action.setShortcutContext(0); //widget context
      this.addAction(action);

      action = qt.QAction.new("&Print it", this);
      action.setShortcut("ctrl+p");
      action.connect("triggered", fun() {
          this.printIt();
      });
      action.setShortcutContext(0); //widget context
      this.addAction(action);

      action = qt.QAction.new("&Inspect it", this);
      action.setShortcut("ctrl+i");
      action.connect("triggered", fun() {
          this.inspectIt();
      });
      action.setShortcutContext(0); //widget context
      this.addAction(action);

      action = qt.QAction.new("De&bug it", this);
      action.setShortcut("ctrl+b");
      action.connect("triggered", fun() {
          this.debugIt();
      });
      action.setShortcutContext(0); //widget context
      this.addAction(action);
    }

    fun doIt() {
      var selection = this.selectedText();

      var CompiledFunction = getClass_CompiledFunction();
      var cmod = get_current_compiled_module();
      var cfun = CompiledFunction.new(selection, [], cmod, @variables);
      var fn = cfun.asContext(thisModule, null, @variables);
      var res =  fn.apply([]);
      @variables = @variables + fn.getEnv();
      return res;
    }
    fun printIt() {
      var res = this.doIt();
      this.insertSelectedText(res.toString());
    }
    fun inspectIt() {
      var res = this.doIt();
      Inspector.new(res).show();
    }
    fun selectedText() {
      return this.textCursor().selectedText();
    }
    fun insertSelectedText(text) {
      var cursor = this.textCursor();
      var pos = cursor.selectionEnd();
      cursor.setPosition(pos);
      cursor.insertText(text);
      cursor.dragRight(text.size());
      this.setTextCursor(cursor);
    }
  }

  class Workspace < QMainWindow {
    fields: editor;
    init new() {
      super.new();
      this.setWindowTitle("Workspace");
      @editor = Editor.new(this);
      this.setCentralWidget(@editor);
      var execMenu = this.menuBar().addMenu("&Execution");
      @editor.actions().each(fun(action) {
        execMenu.addAction(action)
      });
    }
  }

  class Inspector  < QMainWindow {
    fields: inspectee, mirror, fieldList, textArea, lineEdit;
    init new(inspectee) {
      super.new();

      @inspectee = inspectee;
      var Mirror = get_mirror_class();
      @mirror = Mirror.new(@inspectee);

      this.resize(300,250);
      this.setWindowTitle("Inspector");
      var centralWidget = QWidget.new(this);
      var mainLayout = qt.QVBoxLayout.new(centralWidget);

      var hbox = qt.QHBoxLayout.new(null);

      @fieldList = qt.QListWidget.new(centralWidget);
      @fieldList.setMaximumWidth(200);
      hbox.addWidget(@fieldList);

      @textArea = Editor.new(centralWidget);
      hbox.addWidget(@textArea);

      mainLayout.addLayout(hbox);

      @lineEdit = qt.QLineEdit.new(centralWidget);
      @lineEdit.connect("returnPressed", fun() {
          this.lineEditDoIt(@lineEdit.text());
      });
      mainLayout.addWidget(@lineEdit);

      this.setCentralWidget(centralWidget);

      this.loadValues();
      @fieldList.connect("currentItemChanged", fun(item, prev) {
          this.itemSelected(item);
      });
      @fieldList.connect("itemActivated", fun(item) {
          this.itemActivated(item);
      });

      var execMenu = this.menuBar().addMenu("&Execution");

      // action = qt.QAction.new("Accept it", execMenu);
      // action.setShortcut("ctrl+s");
      // action.connect("triggered", fun() {
      //     this.acceptIt();
      // });
      // execMenu.addAction(action);
    }

    fun loadValues() {
      qt.QListWidgetItem.new('*self', @fieldList);
      @mirror.fields().each(fun(name) {
          qt.QListWidgetItem.new(name, @fieldList);
      });
    }

    fun itemSelected(item) { //QListWidgetItem, curr from the signal
      if (item.text() == '*self') {
        @textArea.setPlainText(@inspectee.toSource());
      } else {
        var value = @mirror.valueFor(item.text());
        @textArea.setPlainText(value.toSource());
      }
    }

    fun itemActivated(item) {
      if (item.text() != '*self') {
        var value = @mirror.valueFor(item.text());
        Inspector.new(value).show();
      }
    }

    fun lineEditDoIt(text) {
      var CompiledFunction = getClass_CompiledFunction();
      var cmod = get_current_compiled_module();
      var cfun = CompiledFunction.new(text, [], cmod, {});
      var fn = cfun.asContext(thisModule, @inspectee, {});
      fn.apply([]);
    }

    fun acceptIt() {
      var CompiledFunction = getClass_CompiledFunction();
      var cmod = get_current_compiled_module();
      var cfun = CompiledFunction.new(@textArea.toPlainText(), [], cmod, {});
      var fn = cfun.asContext(thisModule, @inspectee, {});
      var new_value = fn.apply([]);
      var slot = @fieldList.currentItem().text();
      @mirror.setValueFor(slot, new_value);
    }
  }


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
    fun text() {
        <primitive "qt_scintilla_text">
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
    fun localsFor(i) {
      if (i < @vmproc.stackFrames().size()) {
        return @vmproc.stackFrames().get(i).localVars();
      } else {
        return @vmproc.localVars();
      }
    }
    fun moduleVarsFor(i) {
      // var pnames = null;
      // if (i < @vmproc.stackFrames().size()) {
      //   pnames = @vmproc.stackFrames().get(i).modulePointer()._compiledModule().params();
      // } else {
      //   pnames = @vmproc.modulePointer()._compiledModule().params();
      // }
      // var ret = {};
      // pnames.each(fun(name) {
      //   ret[name] = @vmproc.modulePointer.entry(name);
      // });
      return {};
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
    fields: variables;
    init new(parent) {
      super.new(2, 2, parent);
      this.verticalHeader().hide();
      this.setSelectionMode(1);
      var header = this.horizontalHeader();
      header.setStretchLastSection(true);
      this.setSortingEnabled(false);
    }
    fun setVariables(vars) {
      @variables = vars;
      this.clear();
      this.setHorizontalHeaderLabels(['Name', 'Value']);
      var i = 0;
      vars.each(fun(name,value) {
        this.setItem(i, 0, qt.QTableWidgetItem.new(name));
        this.setItem(i, 1, qt.QTableWidgetItem.new(value.toString()));
        i = i + 1;
      });
    }
  }

  class DebuggerUI < QMainWindow {
    fields: eventloop, process, execFrames, stackCombo, editor, localVarList, moduleVarList;
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
          @localVarList.setVariables(@execFrames.localsFor(i));
          @moduleVarList.setVariables(@execFrames.moduleVarsFor(i));
        }
      });

      var hbox = qt.QHBoxLayout.new(null);
      @localVarList = VariableListWidget.new(centralWidget);
      hbox.addWidget(@localVarList);

      @moduleVarList = VariableListWidget.new(centralWidget);
      hbox.addWidget(@moduleVarList);

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

      var action = qt.QAction.new("Compile and &Rewind", execMenu);
      action.setShortcut("ctrl+s");
      action.connect("triggered", fun() {
        this.compileAndRewind()
      });
      execMenu.addAction(action);

      var action = qt.QAction.new("Leave context", execMenu);
      action.setShortcut("ctrl+o");
      action.connect("triggered", fun() {
        //this.leaveContext(); //drop stack frame
      });
      execMenu.addAction(action);

      @stackCombo.updateInfo();
    }
    fun stepInto() {
      @process.stepInto();
      @stackCombo.updateInfo();
    }
    fun stepOver() {
      @process.stepOver();
      @stackCombo.updateInfo();
    }
    fun done() {
      @process.changeState('running');
    }
    fun continue() {
      this.close();
      @process.changeState('running');
      @eventloop.exit(0);
    }
    fun compileAndRewind() {
      var text = @editor.text();
      @process.contextPointer().compiledFunction().setCode(text);
      @process.rewind();
      @stackCombo.updateInfo();
    }
  }

  fun modules_path() {
    <primitive "modules_path">
  }

  class ModuleExplorer < QMainWindow {
    fields: webview;
    init new() {
      super.new();
      this.setWindowTitle("Memetalk");
      @webview = qt.QWebView.new(this);
      @webview.setUrl(modules_path() + "/module-explorer/index.html");
      @webview.page().setLinkDelegationPolicy(2);
      @webview.page().enablePluginsWith("editor", fun(params) {
        var e = Editor.new(null);
        e.setStyleSheet("border-style: outset;");
        e.setPlainText(params["code"]);
        return e;
      });
      this.setCentralWidget(@webview);
      this.resize(800,600);
      @webview.connect('linkClicked', fun(url) {
        if (url == "/tutorial") {
          this.show_tutorial();
        }
        if (url == "/modules") {
          this.show_modules();
        }
      });
    }
    fun show_tutorial() {
      @webview.setUrl(modules_path() + "/module-explorer/tutorial.html");
    }
  }

  fun main() {
    var app = qt.QApplication.new();
    var main = ModuleExplorer.new();
    main.show();
    app.exec();
  }

  fun debug(process) {
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
