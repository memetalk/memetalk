module idez(qt,io)
  qt:  memetalk/qt/1.0();
  io:  memetalk/io/1.0();
  [QWidget, QMainWindow, QsciScintilla, QLineEdit, QComboBox, QTableWidget] <= qt;
{
  fun evalFn(text, imodule, frameOrEnv) {
    var cmod = get_compiled_module(imodule);
    var cfun = CompiledFunction.new(text, [], cmod);
    return cfun.asContext(imodule, frameOrEnv);
  }

  fun evalWithVars(text, imodule, vars) {
    var fn = evalFn(text, imodule, vars);
    var res =  fn.apply([]);
    return {"result":res, "env":fn.getEnv()};
  }

  class LineEditor < QLineEdit {
    fields: receiver;
    init new(parent, receiver) {
      super.new(parent);
      if (parent == null) {
        this.initActions();
      }
      @receiver = receiver;
    }

    fun initActions() {
      this.connect("returnPressed", fun() {
        this.selectAllAndDoit(null);
      });

      var action = qt.QAction.new("&Do it", this);
      action.setShortcut("ctrl+d");
      action.connect("triggered", fun() {
          this.doIt();
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
    fun evalSelection() {
      var r = evalWithVars(this.selectedText(), thisModule, {"this" : @receiver});
      return r["result"];
    }
    fun insertSelectedText(text) {
      var len = this.text().size();
      this.setText(this.text() + text);
      this.setSelection(len, text.size());
    }
    fun selectAllAndDoit(receiver) {
        this.selectAll();
        this.doIt();
    }
    fun doIt() {
      try {
        this.evalSelection();
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    fun printIt() {
      try {
        var res = this.evalSelection();
        this.insertSelectedText(res.toString());
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    fun inspectIt() {
      try {
        var res = this.evalSelection();
        Inspector.new(res).show();
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    fun debugIt() {
      try {
        var fn = evalFn(this.selectedText(), thisModule, {"this" : @receiver});
        VMProcess.debug(fn,[]);
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
  }

  class Editor < QsciScintilla {
    fields: onAccept, getContext, afterEval;
    init new(parent, getContext, afterEval) {
      super.new(parent);
      if (parent == null) {
        this.initActions();
      }
      @getContext = getContext;
      @afterEval = afterEval;
    }

    fun initActions() {
      var action = qt.QAction.new("&Do it", this);
      action.setShortcut("ctrl+d");
      action.connect("triggered", fun() {
          this.doIt();
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

    fun onAccept(fn) {
      if (@onAccept == null) {
        var action = qt.QAction.new("&Accept it", this);
        action.setShortcut("ctrl+s");
        action.connect("triggered", fun() {
          @onAccept();
        });
        action.setShortcutContext(0); //widget context
        this.addAction(action);
      }
      @onAccept = fn;
    }

    fun evalSelection() {
      var r = evalWithVars(this.selectedText(), thisModule, @getContext());
      if (@afterEval) {
        @afterEval(r["env"]);
      }
      return r["result"];
    }

    fun doIt() {
      try {
        this.evalSelection();
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }

    fun printIt() {
      try {
        var res = this.evalSelection();
        this.insertSelectedText(res.toString());
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }

    fun inspectIt() {
      try {
        var res = this.evalSelection();
        Inspector.new(res).show();
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }

    fun debugIt() {
      try {
        var fn = evalFn(this.selectedText(), thisModule, @getContext());
        VMProcess.debug(fn,[]);
        @afterEval(fn.getEnv());
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    fun insertSelectedText(text) {
      var pos = this.getSelection();
      this.insertAt(text, pos["end_line"], pos["end_index"]);
      //text.size(): this is rude
      this.setSelection(pos["end_line"], pos["end_index"], pos["end_line"] + text.count("\n"), pos["end_index"] + text.size());
    }
  }

  class Workspace < QMainWindow {
    fields: editor, variables;
    init new() {
      super.new();
      @variables = {};

      this.setWindowTitle("Workspace");

      @editor = Editor.new(this, fun() { @variables },
                           fun(env) { @variables = env + @variables; });

      @editor.initActions();
      this.setCentralWidget(@editor);

      var execMenu = this.menuBar().addMenu("&Exploring");
      @editor.actions().each(fun(action) {
        execMenu.addAction(action)
      });
    }
  }

  class Inspector  < QMainWindow {
    fields: inspectee, variables, mirror, fieldList, textArea, lineEdit;
    init new(inspectee) {
      super.new();

      @variables = {"this":@inspectee};
      @inspectee = inspectee;
      @mirror = Mirror.new(@inspectee);

      this.resize(300,250);
      this.setWindowTitle("Inspector");
      var centralWidget = QWidget.new(this);
      var mainLayout = qt.QVBoxLayout.new(centralWidget);

      var hbox = qt.QHBoxLayout.new(null);

      @fieldList = qt.QListWidget.new(centralWidget);
      @fieldList.setMaximumWidth(200);
      hbox.addWidget(@fieldList);

      @textArea = Editor.new(centralWidget, fun() { {"this" : @inspectee} }, null);

      hbox.addWidget(@textArea);

      mainLayout.addLayout(hbox);

      @lineEdit = LineEditor.new(centralWidget, @inspectee);
      mainLayout.addWidget(@lineEdit);

      @lineEdit.connect("returnPressed", fun() {
        @lineEdit.selectAllAndDoit(@inspectee);
      });

      this.setCentralWidget(centralWidget);

      this.loadValues();
      @fieldList.connect("currentItemChanged", fun(item, prev) {
          this.itemSelected(item);
      });
      @fieldList.connect("itemActivated", fun(item) {
          this.itemActivated(item);
      });

      var execMenu = this.menuBar().addMenu("&Exploring");

      var action = qt.QAction.new("&Do it", this);
      action.setShortcut("ctrl+d");
      action.connect("triggered", fun() {
          this.doIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("&Print it", this);
      action.setShortcut("ctrl+p");
      action.connect("triggered", fun() {
          this.printIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("&Inspect it", this);
      action.setShortcut("ctrl+i");
      action.connect("triggered", fun() {
          this.inspectIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("De&bug it", this);
      action.setShortcut("ctrl+b");
      action.connect("triggered", fun() {
          this.debugIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("Accept it", execMenu);
      action.setShortcut("ctrl+s");
      action.connect("triggered", fun() {
          this.acceptIt();
      });
      execMenu.addAction(action);
    }

    fun loadValues() {
      qt.QListWidgetItem.new('*self', @fieldList);
      @mirror.fields().each(fun(name) {
          qt.QListWidgetItem.new(name, @fieldList);
      });
    }

    fun itemSelected(item) { //QListWidgetItem, curr from the signal
      if (item.text() == '*self') {
        @textArea.setText(@inspectee.toString());
      } else {
        var value = @mirror.valueFor(item.text());
        @textArea.setText(value.toSource());
      }
    }

    fun itemActivated(item) {
      if (item.text() != '*self') {
        var value = @mirror.valueFor(item.text());
        Inspector.new(value).show();
      }
    }

    fun doIt() {
      if (@lineEdit.hasFocus()) {
        @lineEdit.doIt();
      } else {
        @textArea.doIt();
      }
    }

    fun printIt() {
      if (@lineEdit.hasFocus()) {
        @lineEdit.printIt();
      } else {
        @textArea.printIt();
      }
    }
    fun inspectIt() {
      if (@lineEdit.hasFocus()) {
        @lineEdit.inspectIt();
      } else {
        @textArea.inspectIt();
      }
    }
    fun debugIt() {
      if (@lineEdit.hasFocus()) {
        @lineEdit.debugIt();
      } else {
        @textArea.debugIt();
      }
    }
    fun acceptIt() {
      var fn = evalFn(@textArea.text(), thisModule, {"this":@inspectee});
      var new_value = fn.apply([]);
      var slot = @fieldList.currentItem().text();
      @mirror.setValueFor(slot, new_value);
    }
  }


  class StackCombo < QComboBox {
    fields: frames;
    init new(parent, execframes) {
      super.new(parent);
      @frames = execframes;
    }
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
        frame.contextPointer().compiledFunction().fullName() + ":" + frame.instructionPointer()["start_line"].toString()
      });
    }
    fun codeFor(i) {
      return @vmproc.stackFrames().get(i).contextPointer().compiledFunction().text();
    }
    fun localsFor(i) { // this is used for the local variable list widet
      return @vmproc.stackFrames().get(i).localVars();
    }
    fun frame(i) { // this is used for doIt/printIt/etc.
      return @vmproc.stackFrames().get(i);
    }
    fun moduleVarsFor(i) { // this is used for the module variables list wdiget
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
    fun locationInfoFor(i) {
      return @vmproc.stackFrames().get(i).instructionPointer();
    }
    fun size() {
      return @vmproc.stackFrames().size();
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
    fields: cont_on_exit, frame_index, process, execFrames, stackCombo, editor, localVarList, moduleVarList;
    init new(process) {
      super.new();
      @process = process;

      @cont_on_exit = true;
      @frame_index = 0;

      this.resize(700,250);
      this.setWindowTitle("Debugger");
      var centralWidget = QWidget.new(this);
      var mainLayout = qt.QVBoxLayout.new(centralWidget);

      @execFrames = ExecutionFrames.new(process);

      @stackCombo = StackCombo.new(centralWidget, @execFrames);
      mainLayout.addWidget(@stackCombo);

      @editor = Editor.new(centralWidget, fun() { @execFrames.frame(@frame_index) }, null);

      mainLayout.addWidget(@editor);

      @stackCombo.connect("currentIndexChanged",fun(i) {
        if (0 <= i) {
          @frame_index = i;
          @editor.setText(@execFrames.codeFor(i));
          var locInfo = @execFrames.locationInfoFor(i);
          @editor.pausedAtLine(locInfo["start_line"]-1, locInfo["start_col"], locInfo["end_line"]-1, locInfo["end_col"]);
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
        this.stepInto();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("Step &Over", execMenu);
      action.setShortcut("F7");
      action.connect("triggered", fun() {
        this.stepOver()
      });
      execMenu.addAction(action);

      action = qt.QAction.new("&Continue", execMenu);
      action.setShortcut("F5");
      action.connect("triggered", fun() {
        this.continue()
      });
      execMenu.addAction(action);

      action = qt.QAction.new("Compile and &Rewind", execMenu);
      action.setShortcut("ctrl+s");
      action.connect("triggered", fun() {
        this.compileAndRewind()
      });
      execMenu.addAction(action);

      action = qt.QAction.new("Leave context", execMenu);
      action.setShortcut("ctrl+o");
      action.connect("triggered", fun() {
        //this.leaveContext(); //drop stack frame
      });
      execMenu.addAction(action);

      execMenu = this.menuBar().addMenu("&Exploring");
      action = qt.QAction.new("&Do it", this);
      action.setShortcut("ctrl+d");
      action.connect("triggered", fun() {
        @editor.doIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("&Print it", this);
      action.setShortcut("ctrl+p");
      action.connect("triggered", fun() {
          @editor.printIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("&Inspect it", this);
      action.setShortcut("ctrl+i");
      action.connect("triggered", fun() {
          @editor.inspectIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("De&bug it", this);
      action.setShortcut("ctrl+b");
      action.connect("triggered", fun() {
          @editor.debugIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("Accept it", execMenu);
      action.setShortcut("ctrl+s");
      action.connect("triggered", fun() {
          this.acceptIt();
      });
      execMenu.addAction(action);

      @stackCombo.updateInfo();
    }
    fun closeEvent() {
      if (@cont_on_exit) {
        @process.continue();
      }
    }
    fun stepInto() {
      if(@process.stepInto()) {
        @stackCombo.updateInfo();
      } else {
        @cont_on_exit = false;
        this.close();
      }
    }
    fun stepOver() {
      if(@process.stepOver()) {
        @stackCombo.updateInfo();
      } else {
        @cont_on_exit = false;
        this.close();
      }
    }
    fun continue() {
      this.close();
    }
    fun compileAndRewind() {
      var text = @editor.text();
      @process.contextPointer().compiledFunction().setCode(text);
      @process.rewind();
      @stackCombo.updateInfo();
    }
  }

  class ModuleExplorer < QMainWindow {
    fields: webview;
    init new() {
      super.new();
      this.setWindowTitle("Memetalk");
      @webview = qt.QWebView.new(this);
      this.show_home();
      @webview.page().setLinkDelegationPolicy(2);
      @webview.page().enablePluginsWith("editor", fun(params) {
        var variables = {};

        var e = Editor.new(null, fun() { variables },
                           fun(env) { variables = env + variables;});

        e.setStyleSheet("border-style: outset;");
        if (params.has("code")) {
          e.setText(params["code"]);
        }
        if (params.has("module_function")) {
          this.setupAccept(e, params);
        }
        return e;
      });
      this.setCentralWidget(@webview);
      this.resize(800,600);
      @webview.connect('linkClicked', fun(url) {
        io.print("URL selected: " + url);

        if (url == "/") {
          this.show_home();
          return null;
        }

        if (url == "/tutorial") {
          this.show_tutorial();
          return null;
        }
        if (url == "/modules-index") {
          this.show_modules();
          return null;
        }
        var modules = available_modules();
        var name = url.from(1);
        if (modules.has(name)) {
          this.show_module(name);
          return null;
        }
      });
    }
    fun setupAccept(e, params) {
      e.onAccept(fun() {
        io.print("setting code:" + params["module_name"] + " :: " + params["function_name"]);
        var cfn = get_module(params["module_name"]).compiled_functions()[params["function_name"]];
        try {
          cfn.setCode(e.toPlainText());
        } catch(ex) {
          e.insertSelectedText(ex.value());
        }
      });
    }
    fun show_home() {
      @webview.setUrl(modules_path() + "/module-explorer/index.html");
    }
    fun show_tutorial() {
      @webview.setUrl(modules_path() + "/module-explorer/tutorial.html");
    }
    fun show_modules() {
      @webview.loadUrl(modules_path() + "/module-explorer/modules-index.html");
      var modules = available_modules();
      var ul = @webview.page().mainFrame().documentElement().findFirst("ul.modules");
      modules.each(fun(name) {
        ul.appendInside("<li><a href='/" + name + "'>" + name + "</a></li>")
      });
    }
    fun show_module(name) {
      @webview.loadUrl(modules_path() + "/module-explorer/module-view.html");
      var module = get_module(name);
      var doc = @webview.page().mainFrame().documentElement();
      doc.findFirst(".module_name").setText(module.name());

      var ul = doc.findFirst(".module_parameters");
      module.params().each(fun(p) {
        ul.appendInside("<li>" + p + "</li>");
      });

      var fns = module.compiled_functions();
      fns.each(fun(name,cfn) {
        var div = doc.findFirst(".fun_tpl").clone();
        div.setStyleProperty("display","block");
        div.findFirst(".function_name").setText(cfn.name());
        div.findFirst(".fun_paramslist").setText(cfn.parameters().toString());
        div.findFirst(".fun_body param[name=module_name]").setAttribute("value",module.name());
        div.findFirst(".fun_body param[name=function_name]").setAttribute("value",name);
        div.findFirst(".fun_body param[name=code]").setAttribute("value",cfn.text());
        doc.findFirst(".functions").appendInside(div);
      });
    }
  }

  fun main() {
    var app = qt.QApplication.new();
    var main = ModuleExplorer.new();
    main.show();
    return app.exec();
  }

  fun debug(process) {
    var eventloop = null;
    if (!qt.qapp_running()) {
      eventloop = qt.QApplication.new();
    } else {
      eventloop = qt.QEventLoop.new();
    }

    var dbg = DebuggerUI.new(process);
    dbg.show();
    eventloop.exec();
    io.print("debug:main left loop");
  }
}
