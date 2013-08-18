.license
Copyright (c) 2012-2013 Thiago B. L. Silva <thiago@metareload.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
.endlicense

module idez(qt, io)
  qt : memetalk/qt/1.0();
  io : memetalk/io/1.0();
  [QWidget, QMainWindow, QsciScintilla, QLineEdit, QComboBox, QTableWidget] <= qt;
{
  debug: fun(process, ex) {
    var eventloop = null;
    if (!qt.qapp_running()) {
      eventloop = qt.QApplication.new();
    } else {
      eventloop = qt.QEventLoop.new();
    }

    var dbg = DebuggerUI.new(process, ex, eventloop);
    dbg.show();
    var ret = eventloop.exec();
    process.setDebuggerProcess(null);
    if(ret == 0) {
      return ["continue",null];
    } else {
      return ["continue_throw", null];
    }
  }

  evalWithFrame: fun(text, frame, imod) {
    var cmod = get_compiled_module(imod);
    var code = "fun() {" + text + "}";
    var cfn = CompiledFunction.newClosure(code, thisContext.compiledFunction(), false);
    var fn = cfn.asContextWithFrame(imod, frame);
    var res = fn();
    return {"result": res, "env": fn.getEnv()};
  }

  evalWithVars: fun(text, vars, imod) {
    var fn = evalWithVarsFn(text, vars, imod);
    var res = fn();
    return {"result": res, "env": fn.getEnv()};
  }

  evalWithVarsFn: fun(text, vars, imod) {
    var cmod = get_compiled_module(imod);
    var code = "fun() {" + text + "}";
    var cfn = CompiledFunction.newClosure(code, thisContext.compiledFunction(), false);
    return cfn.asContextWithVars(imod, vars);
  }

  main: fun() {
    var app = qt.QApplication.new();
    var me = ModuleExplorer.new();
    VMProcess.stopOnException();
    //var w = Workspace.new();
    //w.show();
    me.show();
    return app.exec();
  }

  class CommandHistory {
    fields: undo, redo, next;
    init new: fun() {
      @next = null;
      @undo = null;
      @redo = null;
    }
    instance_method add: fun(undo, redo) {
      @undo = undo;
      @redo = redo;
      @next = "undo";
    }
    instance_method redo: fun() {
      if (@next == "redo") {
        @redo();
        @next = "undo";
      }
    }
    instance_method undo: fun() {
      if (@next == "undo") {
        @undo();
        @next = "redo";
      }
    }
  }
  class DebuggerUI < QMainWindow {
    fields: frame_index, process, exception, execFrames, stackCombo, editor, localVarList, moduleVarList, statusLabel, eventloop, execMenu;
    init new: fun(process, ex, eventloop) {
      super.new();
      @process = process;
      @eventloop = eventloop;
      @frame_index = 0;
      @exception = ex;

      this.resize(700,800);
      this.setWindowTitle("Debugger");
      var centralWidget = QWidget.new(this);
      var mainLayout = qt.QVBoxLayout.new(centralWidget);

      @statusLabel = qt.QLabel.new(this.statusBar());
      @statusLabel.setMinimumWidth(300);

      @execFrames = ExecutionFrames.new(process);

      @stackCombo = StackCombo.new(centralWidget, @execFrames);
      mainLayout.addWidget(@stackCombo);

      @editor = Editor.new(centralWidget,
                           null,
                           fun() { @execFrames.frame(@frame_index).modulePointer },
                           fun() { @execFrames.frame(@frame_index) },
                           null);

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

        if (ex) {
          @statusLabel.setText("An exception ocurred: " + ex.value);
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

      action = qt.QAction.new("Reload frame", execMenu);
      action.setShortcut("ctrl+R");
      action.connect("triggered", fun() {
        this.reloadFrame()
      });
      execMenu.addAction(action);

      action = qt.QAction.new("Rewind and go", execMenu);
      action.setShortcut("ctrl+g");
      action.connect("triggered", fun() {
        this.rewindAndGo()
      });
      execMenu.addAction(action);

      action = qt.QAction.new("Run to line", execMenu);
      action.setShortcut("alt+l");
      action.connect("triggered", fun() {
        this.runToLine();
      });
      execMenu.addAction(action);
      @execMenu = execMenu;

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
    instance_method acceptIt: fun() {
      var text = @editor.text();
      @execFrames.frame(@frame_index).contextPointer.compiledFunction.setCode(text);
    }
    instance_method closeEvent: fun() {
      if (@exception) {
        @eventloop.exit(1);
      } else {
        @eventloop.exit(0);
      }
    }
    instance_method continue: fun() {
      this.close();
    }
    instance_method disableActions: fun() {
      @execMenu.actions.each(fun(ac) {
        ac.setEnabled(false);
      });
    }
    instance_method enableActions: fun() {
      @execMenu.actions.each(fun(ac) {
        ac.setEnabled(true);
      });
    }
    instance_method reloadFrame: fun() {
      @exception = null;
      @process.reloadFrame(0);
      @stackCombo.updateInfo();
    }
    instance_method runToLine: fun() {
      @exception = null;
      @statusLabel.setText("Run to line...");
      this.disableActions();
      var pos = @editor.getCursorPosition();
      @process.reloadFrame(pos["line"] + 1);
      @stackCombo.updateInfo();
      this.enableActions();
    }
    instance_method rewindAndGo: fun() {
      @exception = null;
      @statusLabel.setText("Rewinding...");
      this.disableActions();
      var fromFrame = @execFrames.frame(@frame_index);
      var topFrame = @execFrames.topFrame();
      var line = topFrame.instructionPointer["start_line"];
      @process.rewindUntil(fromFrame, line);
      @stackCombo.updateInfo();
      this.enableActions();
    }
    instance_method stepInto: fun() {
      @exception = null;
      @statusLabel.setText("Stepping into...");
      this.disableActions();
      var r = @process.stepInto();
      if(r == false) {
        this.continue();
      }
      @stackCombo.updateInfo();
      this.enableActions();
      if (Mirror.vtFor(r) == Exception) {
        @statusLabel.setText("An exception ocurred: " + r.value);
      } else {
        @statusLabel.setText("done");
      }
    }
    instance_method stepOver: fun() {
      @exception = null;
      @statusLabel.setText("Stepping over...");
      this.disableActions();
      var r = @process.stepOver();
      if(r == false) {
        this.continue();
      }
      @stackCombo.updateInfo();
      this.enableActions();
      if (Mirror.vtFor(r) == Exception) {
        @statusLabel.setText("An exception ocurred: " + r.value);
      } else {
        @statusLabel.setText("done");
      }
    }
  }
  class Editor < QsciScintilla {
    fields: getContext, getIModule, getFrame, afterEval;
    init new: fun(parent, getContext, getIModule, getFrame, afterEval) {
      super.new(parent);
      if (parent == null) {
        this.initActions();
      }
      @getContext = getContext;
      @getIModule = getIModule;
      @getFrame = getFrame;
      @afterEval = afterEval;
    }
    instance_method debugIt: fun() {
      try {
        var fn = null;
        if (@getContext) {
          fn = evalWithVarsFn(this.selectedText(), @getContext(), @getIModule());
        } else {
          fn = evalWithVarsFn(this.selectedText(), @getFrame(), @getIModule());
        }
        VMProcess.debug(fn,[]);
        if (@afterEval) {
          @afterEval(fn.getEnv());
        }
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    instance_method doIt: fun() {
      try {
        this.evalSelection();
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    instance_method evalSelection: fun() {
      var r = null;
      if (@getContext) {
        r = evalWithVars(this.selectedText(), @getContext(), @getIModule());
      } else {
        r = evalWithFrame(this.selectedText(), @getFrame(), @getIModule());
      }
      if (@afterEval) {
        @afterEval(r["env"]);
      }
      return r["result"];
    }
    instance_method initActions: fun() {
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
    instance_method insertSelectedText: fun(text) {
      var pos = this.getSelection();
      this.insertAt(text, pos["end_line"], pos["end_index"]);
      var nl = text.count("\n");
      if (nl == 0) {
        this.setSelection(pos["end_line"], pos["end_index"], pos["end_line"], pos["end_index"] + text.size);
      } else {
        var pl = text.rindex("\n");
        this.setSelection(pos["end_line"], pos["end_index"], pos["end_line"] + nl, text.from(pl).size() - 1);
      }
    }
    instance_method inspectIt: fun() {
      try {
        var res = this.evalSelection();
        Inspector.new(res).show();
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    instance_method printIt: fun() {
      try {
        var res = this.evalSelection();
        this.insertSelectedText(res.toString());
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
  }
  class ExecutionFrames {
    fields: vmproc;
    init new: fun(vmproc) {
      @vmproc = vmproc;
    }
    instance_method codeFor: fun(i) {
      var cp = @vmproc.stackFrames().get(i).contextPointer().compiledFunction();
      if (cp.isTopLevel()) {
        return cp.text();
      } else {
        if (cp.isEmbedded()) {
          return cp.topLevelCompiledFunction().text();
        } else {
          return cp.text();
        }
      }
    }
    instance_method frame: fun(i) { // this is used for doIt/printIt/etc.
      return @vmproc.stackFrames().get(i);
    }
    instance_method topFrame: fun() {
      return @vmproc.stackFrames().get(this.size()-1);
    }
    instance_method localsFor: fun(i) { // this is used for the local variable list widet
      return @vmproc.stackFrames().get(i).localVars();
    }
    instance_method locationInfoFor: fun(i) {
      return @vmproc.stackFrames().get(i).instructionPointer();
    }
    instance_method moduleVarsFor: fun(i) { // this is used for the module variables list wdiget
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
    instance_method names: fun() {
      return @vmproc.stackFrames().map(fun(frame) {
        frame.contextPointer().compiledFunction().fullName() + ":" + frame.instructionPointer()["start_line"].toString()
      });
    }
    instance_method size: fun() {
      return @vmproc.stackFrames().size();
    }
  }
  class ExplorerEditor < Editor {
    fields: cfun;
    init new: fun(cfun, parent, getContext, getIModule, getFrame, afterEval) {
      super.new(parent, getContext, getIModule, getFrame, afterEval);
      @cfun = cfun;
    }
    instance_method accept: fun() {
        try {
          @cfun.setCode(this.text());
        } catch(ex) {
          this.insertSelectedText(ex.value());
        }
    }
    instance_method cfun: fun() {
      return @cfun;
    }
  }
  class Inspector < QMainWindow {
    fields: inspectee, variables, mirror, fieldList, textArea, lineEdit;
    init new: fun(inspectee) {
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

      @textArea = Editor.new(centralWidget,
                             fun() { {"this" : @inspectee} },
                             fun() { thisModule }, null, null);

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
    instance_method acceptIt: fun() {
      var fn = evalWithVarsFn(@textArea.text(), {"this":@inspectee}, thisModule);
      var new_value = fn.apply([]);
      var slot = @fieldList.currentItem().text();
      @mirror.setValueFor(slot, new_value);
    }
    instance_method debugIt: fun() {
      if (@lineEdit.hasFocus()) {
        @lineEdit.debugIt();
      } else {
        @textArea.debugIt();
      }
    }
    instance_method doIt: fun() {
      if (@lineEdit.hasFocus()) {
        @lineEdit.doIt();
      } else {
        @textArea.doIt();
      }
    }
    instance_method inspectIt: fun() {
      if (@lineEdit.hasFocus()) {
        @lineEdit.inspectIt();
      } else {
        @textArea.inspectIt();
      }
    }
    instance_method itemActivated: fun(item) {
      if (item.text() != '*self') {
        var value = @mirror.valueFor(item.text());
        Inspector.new(value).show();
      }
    }
    instance_method itemSelected: fun(item) { //QListWidgetItem, curr from the signal
      if (item.text() == '*self') {
        @textArea.setText(@inspectee.toSource());
      } else {
        var value = @mirror.valueFor(item.text());
        @textArea.setText(value.toSource());
      }
    }
    instance_method loadValues: fun() {
      qt.QListWidgetItem.new('*self', @fieldList);
      @mirror.fields().each(fun(name) {
          qt.QListWidgetItem.new(name, @fieldList);
      });
    }
    instance_method printIt: fun() {
      if (@lineEdit.hasFocus()) {
        @lineEdit.printIt();
      } else {
        @textArea.printIt();
      }
    }
  }
  class LineEditor < QLineEdit {
    fields: receiver;
    init new: fun(parent, receiver) {
      super.new(parent);
      if (parent == null) {
        this.initActions(true);
      }
      @receiver = receiver;
    }
    instance_method debugIt: fun() {
      try {
        var fn = evalWithVarsFn(this.selectedText(), {"this" : @receiver}, thisModule);
        VMProcess.debug(fn,[]);
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    instance_method doIt: fun() {
      try {
        this.evalSelection();
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    instance_method evalSelection: fun() {
      var r = evalWithVars(this.selectedText(), {"this" : @receiver}, thisModule);
      return r["result"];
    }
    instance_method initActions: fun(initReturnPressed) {
      if (initReturnPressed) {
        this.connect("returnPressed", fun() {
          this.selectAllAndDoit(null);
        });
      }

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
    instance_method insertSelectedText: fun(text) {
      var len = this.text().size();
      this.setText(this.text() + text);
      this.setSelection(len, text.size());
    }
    instance_method inspectIt: fun() {
      try {
        var res = this.evalSelection();
        Inspector.new(res).show();
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    instance_method printIt: fun() {
      try {
        var res = this.evalSelection();
        this.insertSelectedText(res.toString());
      } catch(e) {
        this.insertSelectedText(e.value());
      }
    }
    instance_method selectAllAndDoit: fun(receiver) {
        this.selectAll();
        this.doIt();
    }
  }
  class MiniBuffer < QWidget {
    fields: label, lineEdit, callback;
    init new: fun(parent) {
      super.new(parent);

      this.hide();
      this.setMaximumHeight(30);
      @callback = null;
      @lineEdit = LineEditor.new(this, thisModule);
      @lineEdit.initActions(false);
      @lineEdit.setMinimumSize(200,30);
      @label = qt.QLabel.new(this);
      var l = qt.QHBoxLayout.new(this);
      l.addWidget(@label);
      l.addWidget(@lineEdit);
      l.setContentsMargins(10,2,10,2);

      @lineEdit.connect("returnPressed", fun() {
        if (@callback) {
          var close = @callback(@lineEdit.text());
          if (close) { this.hide(); }
        }
      });
    }
    instance_method prompt: fun(labelText, defaultValue, callback) {
      @callback = callback;
      @label.setText(labelText);
      @lineEdit.setText(defaultValue.toString());
      this.show();
      @lineEdit.setFocus();
    }
  }
  class ModuleExplorer < QMainWindow {
    fields: webview, miniBuffer, current_cmodule, imodule, chistory, statusLabel, variables, sysmenu;
    init new: fun() {
      super.new();

      @chistory = CommandHistory.new();
      @variables = {};

      this.setWindowTitle("Memetalk: ModuleExplorer");
      this.resize(1000,600);

      var centralWidget = QWidget.new(this);
      this.setCentralWidget(centralWidget);

      var l = qt.QVBoxLayout.new(centralWidget);
      @webview = qt.QWebView.new(centralWidget);
      l.addWidget(@webview);

      @miniBuffer = MiniBuffer.new(centralWidget);
      l.addWidget(@miniBuffer);

      @statusLabel = qt.QLabel.new(this.statusBar());
      @statusLabel.setMinimumWidth(300);

      @webview.page().setLinkDelegationPolicy(2);

      @webview.page().enablePluginsWith("editor", fun(params) {

        var cfn = null;
        var e = null;
        if (params.has("function_type")) {
          if (params["function_type"] == "module") {
            cfn = get_module(params["module_name"]).compiled_functions()[params["function_name"]];
          }
          if (params["function_type"] == "ctor_method") {
            cfn = get_module(params["module_name"]).compiled_classes()[params["class_name"]].constructors()[params["function_name"]];
          }
          if (params["function_type"] == "class_method") {
            cfn = get_module(params["module_name"]).compiled_classes()[params["class_name"]].classMethods()[params["function_name"]];
          }
          if (params["function_type"] == "instance_method") {
            cfn = get_module(params["module_name"]).compiled_classes()[params["class_name"]].instanceMethods()[params["function_name"]];
          }
          if (cfn) {
            e = ExplorerEditor.new(cfn, null, fun() { @variables },
                                   fun() { this.currentIModule() },
                                   null,
                                   fun(env) { @variables = env + @variables;});
          }
        } else {
          if (params.has("code")) {
            e = ExplorerEditor.new(null, null, fun() { @variables },
                                   fun() { this.currentIModule() },
                                   null,
                                   fun(env) { @variables = env + @variables;});
          }
        }
        if (e) {
          e.setText(params["code"]);
          e.setStyleSheet("border-style: outset;");
        }
        return e;
      });

      @webview.connect('linkClicked', fun(url) {
        io.print("URL selected: " + url.toString());

        if (url.toString() == "memetalk:/") {
          this.show_home();
          return null;
        }

        if (url.toString() == "memetalk:/tutorial") {
          this.show_tutorial();
          return null;
        }
        if (url.toString() == "memetalk:/modules-index") {
          this.show_modules();
          return null;
        }
        if (url.hasFragment()) {
          @webview.page().mainFrame().scrollToAnchor(url.fragment());
          return null;
        }

        var modules = available_modules();
        var name = url.toString().from(1);
        if (modules.has(name)) {
          this.show_module(name);
          return null;
        }
      });

      this.initActions();
      this.show_home();
    }
    instance_method action_acceptCode: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      var e = qt.QApplication.focusWidget();
      if (Mirror.vtFor(e) == ExplorerEditor) {
        e.accept();
        @statusLabel.setText("Code changed");
      } else {
        @statusLabel.setText("No function selected");
      }
    }
    instance_method action_addClass: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      @miniBuffer.prompt("New class: ", "", fun(name) {

        @miniBuffer.prompt("Super class name: ", "Object", fun(super_name) {
          this.command(fun() {
            @current_cmodule.newClass(name, super_name);
            this.show_module(@current_cmodule.name);
            @statusLabel.setText("Class added: " + name);
          }, fun() {
            @current_cmodule.removeClass(name);
            this.show_module(@current_cmodule.name);
            @statusLabel.setText("Class removed: " + name);
          });
        });
        return false;
      });
    }
    instance_method action_addFunction: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      @miniBuffer.prompt("Function name: ", "", fun(name) {
        // TODO, check if function already exists

        var cfun = CompiledFunction.newTopLevel(name, "fun() { return null; }", @current_cmodule, :module_function);
        var doc = @webview.page().mainFrame().documentElement();
        var mlist = doc.findFirst("#menu-listing .link-list");
        this.command(fun() {
          @current_cmodule.addFunction(cfun);
          this.showEditorForFunction(cfun, "module", ".module-functions");
          @statusLabel.setText("Function added: " + name);
          mlist.appendInside("<li><a href='#" + cfun.fullName + "'>" + cfun.fullName + "</a></li>");
        }, fun() {
          mlist.findFirst("li a[href='#" + cfun.fullName + "']").setAttribute("style","display:none");
          @current_cmodule.removeFunction(name);
          @webview.page().mainFrame().documentElement().findFirst("div[id='" + cfun.fullName + "']").takeFromDocument();
          @statusLabel.setText("Function removed: " + name);
        });
      });
    }
    instance_method action_addMethod: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      @miniBuffer.prompt("Method name (ex: Foo.bar, Foo::bar): ", "", fun(spec_name) {
        var flag = null;
        var split = null;
        if (spec_name.count(".") == 1) { //instance method
          split = spec_name.split(".");
          flag = :instance_method;
        } else { //class method
          split = spec_name.split("::");
          flag = :class_method;
        }
        var class_name = split[0];
        var method_name = split[1];

        if (!@current_cmodule.compiled_classes.has(class_name)) {
          @statusLabel.setText("No class found: " + class_name);
          return true;
        }

        //TODO, check if method already exists

        var klass = @current_cmodule.compiled_classes[class_name];
        var cfun = CompiledFunction.newTopLevel(method_name, "fun() { return null; }", klass, flag);
        var doc = @webview.page().mainFrame().documentElement();
        var mlist = doc.findFirst("#menu-listing .link-list");
        this.command(fun() {
          klass.addMethod(cfun, flag);
          this.showEditorForFunction(cfun, flag.toString, "div[id='imethods_" + klass.name + "']");
          @statusLabel.setText("Method added: " + cfun.fullName);
          mlist.appendInside("<li><a href='#" + cfun.fullName + "'>" + cfun.fullName + "</a></li>");
        }, fun() {
          mlist.findFirst("li a[href='#" + cfun.fullName + "']").setAttribute("style","display:none");
          klass.removeMethod(method_name, flag);
          doc.findFirst("div[id='" + cfun.fullName + "']").takeFromDocument();
          @statusLabel.setText("Method removed: " + cfun.fullName);
        });
      });

    }
    instance_method action_deleteClass: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      @miniBuffer.prompt("Delete class: ", "", fun(name) {
        if (!@current_cmodule.compiled_classes.has(name)) {
          @statusLabel.setText("No class found: " + name);
          return true;
        }
        var klass = @current_cmodule.compiled_classes[name];
        this.command(fun() {
          @current_cmodule.removeClass(name);
          this.show_module(@current_cmodule.name);
        }, fun() {
          @current_cmodule.addClass(klass);
          this.show_module(@current_cmodule.name);
        });
      });
    }
    instance_method action_deleteFunction: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      var e = qt.QApplication.focusWidget();
      if (Mirror.vtFor(e) == ExplorerEditor) {
        var cfun = e.cfun();
        var doc = @webview.page().mainFrame().documentElement();
        var div = doc.findFirst("div[id='" + cfun.fullName + "']");
        var mitem = doc.findFirst("#menu-listing .link-list").findFirst("li a[href='#" + cfun.fullName + "']");

        var owner = cfun.owner;
        var flag = null;
        this.command(fun() {
          if (Mirror.vtFor(owner) == CompiledModule) {
            owner.removeFunction(cfun.name);
          } else {
            flag = owner.methodFlag(cfun);
            owner.removeMethod(cfun.name, flag);
          }
          div.setAttribute("style","display:none");
          mitem.setAttribute("style","display:none");
          @statusLabel.setText("Function deleted: " + cfun.name);
        }, fun() {
          if (Mirror.vtFor(owner) == CompiledModule) {
            owner.addFunction(cfun);
          } else {
            owner.addMethod(cfun, flag);
          }
          div.setAttribute("style","display:block");
          mitem.setAttribute("style","display:block");
          @statusLabel.setText("Function added: " + cfun.name);
        });
      } else {
        @statusLabel.setText("No function selected");
      }
    }
    instance_method action_editFields: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      @miniBuffer.prompt("Edit fields for class: ", "", fun(name) {
        if (!@current_cmodule.compiled_classes.has(name)) {
          @statusLabel.setText("No class found: " + name);
          return true;
        }
        var klass = @current_cmodule.compiled_classes[name];
        var doc = @webview.page().mainFrame().documentElement();
        var old_fields = klass.fields;
        @miniBuffer.prompt("Fields: ", klass.fields.toString, fun(fields) {
          this.command(fun() {
            klass.setFields(fields.split(","));
            doc.findFirst("div[id='" + klass.fullName + "'] .fields_list").setPlainText(fields.toString());
          }, fun() {
            klass.setFields(old_fields);
            doc.findFirst("div[id='" + klass.fullName + "'] .fields_list").setPlainText(old_fields.toString());
          });
        });
        return false;
      });
    }
    instance_method action_editModuleDefaultParameters: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }
      @miniBuffer.prompt("Edit default parameter for: ", "", fun(name) {
        if (!@current_cmodule.params().has(name)) {
          @statusLabel.setText("parameter not found:" + name);
          return true;
        }
        var dp = @current_cmodule.defaultParameterFor(name);
        var doc = @webview.page().mainFrame().documentElement();
        @miniBuffer.prompt("value: ", dp.toString, fun(val) {
          this.command(fun() {
            @current_cmodule.setDefaultParameter(name, val);
            var lis = @current_cmodule.default_parameters.map(fun(name,d) { "<li>" + name + " : memetalk/" + d["value"] + "/1.0</li>"; });
            doc.findFirst(".default-parameters").setInnerXml(lis.join(""));
          }, fun() {
            @current_cmodule.setDefaultParameter(name, dp);
            var lis = @current_cmodule.default_parameters.map(fun(name,d) { "<li>" + name + " : memetalk/" + d["value"] + "/1.0</li>"; });
            doc.findFirst(".default-parameters").setInnerXml(lis.join(""));
          });
        });
        return false;
      });
    }
    instance_method action_editModuleParameters: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      var old_params = @current_cmodule.params;
      var doc = @webview.page().mainFrame().documentElement();
      @miniBuffer.prompt("Edit module parameters: ", old_params.toString(), fun(params) {
        this.command(fun() {
          @current_cmodule.setParams(params.split(","));
          doc.findFirst(".module_title_params").setPlainText(params);
        }, fun() {
          @current_cmodule.setParams(old_params);
          doc.findFirst(".module_title_params").setPlainText(old_params.toString);
        });
      });
    }
    instance_method action_eval: fun() {
      @miniBuffer.prompt("eval: ", "", fun(expr) {
        try {
          var imod = null;
          if (@imodule) {
            imod = @imodule;
          } else {
            imod = thisModule;
          }
          var r = evalWithVars(expr, {}, imod);
          @statusLabel.setText(r["result"].toString());
        } catch(e) {
          @statusLabel.setText(e.value);
        }
      });
    }
    instance_method action_evalUntil: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return null;
      }
      var e = qt.QApplication.focusWidget();
      if (Mirror.vtFor(e) != ExplorerEditor) {
        @statusLabel.setText("No function selected");
        return null;
      }
      @miniBuffer.prompt("Eval*: ", "", fun(expr) {
        try {
          var imod = null;
          if (@imodule) {
            imod = @imodule;
          } else {
            imod = thisModule;
          }

          break_at(e.cfun, e.getCursorPosition["line"] + 1);
          var r = evalWithVars(expr, {}, imod);
          @statusLabel.setText(r["result"].toString());
        } catch(ex) {
          @statusLabel.setText(ex.value);
        }
      });
}
    instance_method action_instantiateModule: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }
      @imodule = @current_cmodule.instantiate({});
      @statusLabel.setText("Module loaded");
    }
    instance_method action_renameClass: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      @miniBuffer.prompt("Rename class: ", "", fun(name) {
        if (!@current_cmodule.compiled_classes.has(name)) {
          @statusLabel.setText("No class found: " + name);
          return true;
        }
        var doc = @webview.page().mainFrame().documentElement();
        var klass = @current_cmodule.compiled_classes[name];
        @miniBuffer.prompt("to: ", "", fun(new_name) {
          this.command(fun() {
            klass.rename(new_name);
            this.show_module(@current_cmodule.name);
            @statusLabel.setText("Class " + name + " renamed to " + new_name);
          }, fun() {
            klass.rename(name);
            this.show_module(@current_cmodule.name);
            @statusLabel.setText("Class " + new_name + " renamed to " + name);
          });
        });
        return false;
      });
    }
    instance_method action_reset: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      var e = qt.QApplication.focusWidget();
      if (Mirror.vtFor(e) == ExplorerEditor) {
        var cfun = e.cfun();
        e.setText(cfun.text());
        @statusLabel.setText("Function " + cfun.fullName + " was reset");
      }
    }
    instance_method action_saveToFileSystem: fun() {
      available_modules().each(save_module);
    }
    instance_method action_toggleCtor: fun() {
      if (@current_cmodule == null) {
        @statusLabel.setText("No current module");
        return true;
      }

      var e = qt.QApplication.focusWidget();
      if (Mirror.vtFor(e) == ExplorerEditor) {
        e.cfun.setCtor(!e.cfun.is_constructor());
        if (e.cfun.is_constructor()) {
          @statusLabel.setText(e.cfun.fullName + " now is a constructor");
        } else {
          @statusLabel.setText(e.cfun.fullName + " now is not a constructor");
        }
      } else {
        @statusLabel.setText("No function selected");
      }
    }
    instance_method command: fun(redo, undo) {
      redo();
      @chistory.add(undo,redo);
    }
    instance_method currentIModule: fun() {
      if (@imodule) {
        return @imodule;
      }
      return thisModule;
    }
    instance_method initActions: fun() {

      var action = qt.QAction.new("Inspect", this);
      action.setShortcut("alt+w,i");
      action.connect("triggered", fun() {
        Inspector.new(this).show()
      });
      action.setShortcutContext(1);
      this.addAction(action);

      // System menu
      var execMenu = this.menuBar().addMenu("System");
      @sysmenu = execMenu;
      action = qt.QAction.new("&Save", execMenu);
      action.setShortcut("alt+x,s");
      action.connect("triggered", fun() {
        this.action_saveToFileSystem();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("&Accept code", execMenu);
      action.setShortcut("alt+x,a");
      action.connect("triggered", fun() {
        this.action_acceptCode();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Eval", execMenu);
      action.setShortcut("ctrl+e");
      action.connect("triggered", fun() {
        this.action_eval();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Eval Until", execMenu);
      action.setShortcut("alt+e");
      action.connect("triggered", fun() {
        this.action_evalUntil();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Dismiss Mini Buffer", execMenu);
      action.setShortcut("ctrl+g");
      action.connect("triggered", fun() {
        @miniBuffer.hide();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Undo", execMenu);
      action.setShortcut("alt+u");
      action.connect("triggered", fun() {
        @chistory.undo();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Redo", execMenu);
      action.setShortcut("alt+r");
      action.connect("triggered", fun() {
        @chistory.redo();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Reset", execMenu);
      action.setShortcut("alt+x,r");
      action.connect("triggered", fun() {
        this.action_reset()
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      // Edit Menu

      execMenu = this.menuBar().addMenu("Module");

      action = qt.QAction.new("&Instantiate module", execMenu);
      action.setShortcut("alt+m,l");
      action.connect("triggered", fun() {
        this.action_instantiateModule();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("&Rename module", execMenu);
      action.setShortcut("alt+m,r");
      action.connect("triggered", fun() {
        io.print("rename module");
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Edit Module &Parameters", execMenu);
      action.setShortcut("alt+m,p");
      action.connect("triggered", fun() {
        this.action_editModuleParameters();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Edit Module &Default Parameters", execMenu);
      action.setShortcut("alt+m,d");
      action.connect("triggered", fun() {
        this.action_editModuleDefaultParameters();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Add Module &Function", execMenu);
      action.setShortcut("alt+m,f");
      action.connect("triggered", fun() {
        this.action_addFunction()
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      var del_fn_ac = qt.QAction.new("&Delete Function", execMenu);
      del_fn_ac.setShortcut("alt+f,d");
      del_fn_ac.connect("triggered", fun() {
        this.action_deleteFunction()
      });
      del_fn_ac.setShortcutContext(1);
      execMenu.addAction(del_fn_ac);

      // Class Menu

      execMenu = this.menuBar().addMenu("Class");
      action = qt.QAction.new("&Add Class", execMenu);
      action.setShortcut("alt+c,a");
      action.connect("triggered", fun() {
        this.action_addClass();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Delete Class", execMenu);
      action.setShortcut("alt+c,k");
      action.connect("triggered", fun() {
        this.action_deleteClass();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Rename Class", execMenu);
      action.setShortcut("alt+c,r");
      action.connect("triggered", fun() {
        this.action_renameClass();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Edit &Fields", execMenu);
      action.setShortcut("alt+c,f");
      action.connect("triggered", fun() {
        this.action_editFields();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      action = qt.QAction.new("Add &Method", execMenu);
      action.setShortcut("alt+c,m");
      action.connect("triggered", fun() {
        this.action_addMethod();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);

      execMenu.addAction(del_fn_ac);

      action = qt.QAction.new("Toggle Method as Constructor", execMenu);
      action.setShortcut("alt+c,c");
      action.connect("triggered", fun() {
        this.action_toggleCtor();
      });
      action.setShortcutContext(1);
      execMenu.addAction(action);
    }
    instance_method show_home: fun() {
      @current_cmodule = null;
      @imodule = null;
      @webview.setUrl(modules_path() + "/module-explorer/index.html");
    }
    instance_method show_module: fun(name) {
      @current_cmodule = get_module(name);
      @webview.loadUrl(modules_path() + "/module-explorer/module-view.html");
      var doc = @webview.page().mainFrame().documentElement();
      doc.findFirst("head").appendInside('<link rel="stylesheet" href="file://' + modules_path() + '/module-explorer/style.css" type="text/css">');

      doc.findFirst("#module_title").setPlainText(@current_cmodule.name());

      doc.findFirst(".module_title_params").setPlainText(@current_cmodule.params.toString());

      var dp = @current_cmodule.default_parameters.map(fun(name,d) { "<li>" + name + " : memetalk/" + d["value"] + "/1.0</li>"; });
      doc.findFirst(".default-parameters").appendInside(dp.join(""));

      var mlist = doc.findFirst("#menu-listing .link-list");

      var fns = @current_cmodule.compiled_functions();
      fns.each(fun(name,cfn) {
        mlist.appendInside("<li><a href='#" + cfn.fullName + "'>" + cfn.fullName + "</a></li>");
        this.showEditorForFunction(cfn, "module", ".module-functions");
      });

      @current_cmodule.compiled_classes().each(fun(name, klass) {
        this.showClass(klass);
      });
    }
    instance_method show_modules: fun() {
      @current_cmodule = null;
      @imodule = null;
      @webview.loadUrl(modules_path() + "/module-explorer/modules-index.html");
      var modules = available_modules();
      var ul = @webview.page().mainFrame().documentElement().findFirst("ul.modules");
      modules.each(fun(name) {
        ul.appendInside("<li><a href='/" + name + "'>" + name + "</a></li>")
      });
    }
    instance_method show_tutorial: fun() {
      @current_cmodule = null;
      @imodule = null;
      @webview.setUrl(modules_path() + "/module-explorer/tutorial.html");
    }
    instance_method showClass: fun(klass) {
      var doc = @webview.page().mainFrame().documentElement();
      var div = doc.findFirst("div[id=templates] .class_template").clone();
      doc.findFirst(".module-classes").appendInside(div);
      div.setStyleProperty("display","block");
      div.setAttribute("id", klass.fullName);

      div.findFirst(".class_name").setPlainText(klass.name);
      div.findFirst(".super_class").setPlainText(klass.super_class_name);

      div.findFirst(".fields_list").setPlainText(klass.fields.toString());

      var mlist = doc.findFirst("#menu-listing .link-list");

      var ctors = div.findFirst(".constructors");
      ctors.setAttribute("id", "ctors_" + klass.name);

      klass.constructors.each(fun(name, cfn) {
        mlist.appendInside("<li><a href='#" + cfn.fullName + "'>" + cfn.fullName + "</a></li>");
        this.showEditorForFunction(cfn, "ctor_method", "div[id='ctors_" + klass.name + "']");
      });
      var ims = div.findFirst(".instance_methods");
      ims.setAttribute("id", "imethods_" + klass.name);

      klass.instanceMethods.each(fun(name, cfn) {
        mlist.appendInside("<li><a href='#" + cfn.fullName + "'>" + cfn.fullName + "</a></li>");
        this.showEditorForFunction(cfn, "instance_method", "div[id='imethods_" + klass.name + "']");
      });

      var cms = div.findFirst(".class_methods");
      cms.setAttribute("id", "cmethods_" + klass.name);

      klass.classMethods.each(fun(name, cfn) {
        mlist.appendInside("<li><a href='#" + cfn.fullName + "'>" + cfn.fullName + "</a></li>");
        this.showEditorForFunction(cfn, "class_method", "div[id='cmethods_" + klass.name + "']");
      });
    }
    instance_method showEditorForFunction: fun(cfn, function_type, parent_sel) {
      var doc = @webview.page().mainFrame().documentElement();
      var parent = doc.findFirst(parent_sel);
      var div = doc.findFirst("div[id=templates] .function_template").clone();
      div.setAttribute("id", cfn.fullName);
      div.setStyleProperty("display","block");
      div.findFirst(".function_name").setPlainText(cfn.fullName);
      div.findFirst(".function_name").setAttribute("name",cfn.fullName);
      div.findFirst(".function-source-code param[name=function_type]").setAttribute("value",function_type);
      div.findFirst(".function-source-code param[name=module_name]").setAttribute("value",@current_cmodule.name());
      div.findFirst(".function-source-code param[name=function_name]").setAttribute("value",cfn.name);
      if (Mirror.vtFor(cfn.owner) == CompiledClass) {
        div.findFirst(".function-source-code param[name=class_name]").setAttribute("value",cfn.owner.name);
      }
      div.findFirst(".function-source-code param[name=code]").setAttribute("value",cfn.text());
      parent.appendInside(div);
    }
  }
  class StackCombo < QComboBox {
    fields: frames;
    init new: fun(parent, execframes) {
      super.new(parent);
      @frames = execframes;
    }
    instance_method updateInfo: fun() {
      this.clear();
      @frames.names().each(fun(name) {
        this.addItem(name);
      });
      this.setCurrentIndex(@frames.size() - 1);
    }
  }
  class VariableListWidget < QTableWidget {
    fields: variables;
    init new: fun(parent) {
      super.new(2, 2, parent);
      this.verticalHeader().hide();
      this.setSelectionMode(1);
      var header = this.horizontalHeader();
      header.setStretchLastSection(true);
      this.setSortingEnabled(false);
    }
    instance_method setVariables: fun(vars) {
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
  class Workspace < QMainWindow {
    fields: editor, variables;
    init new: fun() {
      super.new();
      @variables = {};

      this.setWindowTitle("Workspace");

      @editor = Editor.new(this, fun() { @variables },
                           fun() { thisModule }, null,
                           fun(env) { @variables = env + @variables; });

      @editor.initActions();
      this.setCentralWidget(@editor);

      var execMenu = this.menuBar().addMenu("&Exploring");
      @editor.actions().each(fun(action) {
        execMenu.addAction(action)
      });
    }
  }
}
