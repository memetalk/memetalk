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

.preamble(qt, io)
  qt : memetalk/qt/1.0();
  io : memetalk/io/1.0();
  [QWidget, QMainWindow, QsciScintilla, QLineEdit, QComboBox, QTableWidget, QTableWidgetItem] <= qt;

.code

// -- module functions --

debug: fun(process) {
  io.print("::in idez:debug()");
  process.handShakeTarget();
  var app = qt.QApplication.new;
  var dbg = DebuggerUI.new(process);
  dbg.show();
  return app.exec();
}


main: fun() {
  VMProcess.current.debugOnException();
  var app = qt.QApplication.new();
  var me = ModuleExplorer.new();
  me.show();
  return app.exec();
}

// -- module classes --

class CommandHistory
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

end //idez:CommandHistory

class DebuggerEditor < Editor
fields: process, frame_index;
init new: fun(parent, process) {
  super.new(parent);
  @process = process;
}
instance_method setFrameIndex: fun(idx) {
  @frame_index = idx;
}
instance_method doIt: fun() {
  try {
    @process.evalInFrame(this.selectedText(), @frame_index);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}
instance_method printIt: fun() {
  try {
    var res = @process.evalInFrame(this.selectedText(), @frame_index);
    this.insertSelectedText(res.toString());
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method inspectIt: fun() {
  try {
    var res = @process.evalInFrame(this.selectedText(), @frame_index);
    Inspector.inspect(res); //TODO: use a RemoteInspector able to wire changes
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}
end //idez:DebuggerEditor

class DebuggerUI < QMainWindow
fields: frame_index, process, exception, execFrames, stackCombo, editor, localVarList, fieldVarList, statusLabel, execMenu, shouldUpdateVars, continued;
init new: fun(process) {
  super.new();
  @process = process;
  @frame_index = 0;

  @continued = false;

  @shouldUpdateVars = false;

  this.resize(700,800);
  this.setWindowTitle("Debugger");
  var centralWidget = QWidget.new(this);
  var mainLayout = qt.QVBoxLayout.new(centralWidget);

  @statusLabel = qt.QLabel.new(this.statusBar());
  @statusLabel.setMinimumWidth(600);

  if (@process.lastException) {
    @statusLabel.setText(@process.lastException.message);
  }
  @execFrames = ExecutionFrames.new(process);

  @stackCombo = StackCombo.new(centralWidget, @execFrames);
  mainLayout.addWidget(@stackCombo);

  @editor = DebuggerEditor.new(centralWidget, @process);

  @stackCombo.connect("currentIndexChanged",fun(i) {
    @frame_index = i;
    this.updateInfo();
  });

  mainLayout.addWidget(@editor);

  var hbox = qt.QHBoxLayout.new(null);
  @localVarList = VariableListWidget.new(@process, centralWidget);
  hbox.addWidget(@localVarList);


  @fieldVarList = VariableListWidget.new(@process, centralWidget);
  hbox.addWidget(@fieldVarList);


  mainLayout.addLayout(hbox);
  this.setCentralWidget(centralWidget);

  var execMenu = this.menuBar().addMenu("Debug");
  @execMenu = execMenu;
  var action = qt.QAction.new("Step Into", execMenu);
  action.setShortcut("F6");
  action.connect("triggered", fun() {
    this.stepInto();
  });
  execMenu.addAction(action);

  action = qt.QAction.new("Step Over", execMenu);
  action.setShortcut("F7");
  action.connect("triggered", fun() {
    this.stepOver()
  });
  execMenu.addAction(action);

  action = qt.QAction.new("Continue", execMenu);
  action.setShortcut("F5");
  action.connect("triggered", fun() {
    this.continue()
  });
  execMenu.addAction(action);

  action = qt.QAction.new("Reload Frame", execMenu);
  action.setShortcut("ctrl+r");
  action.connect("triggered", fun() {
    this.reloadFrame()
  });
  execMenu.addAction(action);

  action = qt.QAction.new("Rewind And Go", execMenu);
  action.setShortcut("ctrl+g");
  action.connect("triggered", fun() {
    this.rewindAndGo()
  });
  execMenu.addAction(action);

  action = qt.QAction.new("Run To Line", execMenu);
  action.setShortcut("ctrl+l");
  action.connect("triggered", fun() {
    this.runToLine();
  });
  execMenu.addAction(action);

  action = qt.QAction.new("Toggle Var Update", execMenu);
  action.setShortcut("ctrl+u");
  action.connect("triggered", fun() {
    this.toggleVarUpdate();
  });
  execMenu.addAction(action);

  execMenu = this.menuBar().addMenu("Edit");
  @editor.editingActions.each(fun(ac) {
    execMenu.addAction(ac);
  });
  execMenu = this.menuBar().addMenu("Explore");
  @editor.exploringActions.each(fun(ac) {
    execMenu.addAction(ac);
  });

  action = qt.QAction.new("Accept It", execMenu);
  action.setShortcut("ctrl+x,a");
  action.connect("triggered", fun() {
    this.acceptIt();
  });
  execMenu.addAction(action);

  @stackCombo.updateInfo();
}
//end idez:DebuggerUI:new

instance_method acceptIt: fun() {
  var text = @editor.text();
  try {
    var cfun = @execFrames.frame(@frame_index).contextPointer.compiledFunction;
    cfun.setCode(text);
    @editor.saved();
  } catch(ex) {
    @editor.insertSelectedText(ex.message());
  }
}

instance_method continue: fun() {
  @continued = true;
  this.hide();
  @process.continue();
  // if @process.continue return, means were are active  again
  @continued = false;
  @stackCombo.updateInfo();
  this.show();
}

instance_method reloadFrame: fun() {
  @process.reloadFrame(0);
  @stackCombo.updateInfo();
}

instance_method rewindAndGo: fun() {
  var line = @execFrames.topFrame.instructionPointer["start_line"];
  @process.rewindAndBreak(@stackCombo.count - @frame_index, line);
  @stackCombo.updateInfo();
}

instance_method runToLine: fun() {
  @statusLabel.setText("Run to line...");
  this.disableActions();
  var pos = @editor.getCursorPosition();
  @process.reloadFrame(pos["line"] + 1);
  @stackCombo.updateInfo();
  this.enableActions();
}

instance_method save: fun() {
  available_modules().each(save_module);
}

instance_method stepInto: fun() {
  @process.stepInto();
  @stackCombo.updateInfo();
}

instance_method stepOver: fun() {
  @process.stepOver();
  @stackCombo.updateInfo();
}

instance_method toggleVarUpdate: fun() {
  @shouldUpdateVars = !@shouldUpdateVars;
  this.updateInfo();
}

instance_method updateInfo: fun() {
  if (0 <= @frame_index) {
    @editor.setText(@execFrames.codeFor(@frame_index));
    @editor.setFrameIndex(@frame_index);
    var locInfo = @execFrames.locationInfoFor(@frame_index);
    @editor.pausedAtLine(locInfo["start_line"], locInfo["start_col"], locInfo["end_line"], locInfo["end_col"]);
    @localVarList.clear();
    @fieldVarList.clear();
    if (@shouldUpdateVars) {
      @localVarList.loadFrame(@execFrames.frame(@frame_index));
      @fieldVarList.loadReceiver(@execFrames.frame(@frame_index));
    }
  }
}
instance_method closeEvent: fun() {
  if (!@continued) {
    @process.detach();
  }
}
end //idez:DebuggerUI

class Editor < QsciScintilla
fields: editing_actions, exploring_actions;
init new: fun(parent) {
  super.new(parent);
  this.initActions();
}

instance_method appendSelectedText: fun(text) {
  this.append("\n" + text);
  var nl = text.count("\n") + 1;
  this.setSelection(this.lines - nl, 0, this.lines, 0);
}

instance_method editingActions: fun() {
  return @editing_actions;
}

instance_method exploringActions: fun() {
  return @exploring_actions;
}

instance_method initActions: fun() {

  @editing_actions = [];
  @exploring_actions = [];

  // editing actions
  var action = qt.QAction.new("Cut", this);
  action.setShortcut("ctrl+w");
  action.connect("triggered", fun() {
      this.cut();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  action = qt.QAction.new("Copy", this);
  action.setShortcut("alt+w");
  action.connect("triggered", fun() {
      this.copy();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  action = qt.QAction.new("Paste", this);
  action.setShortcut("ctrl+y");
  action.connect("triggered", fun() {
      this.paste();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  action = qt.QAction.new("Undo", this);
  action.setShortcut("ctrl+shift+-");
  action.connect("triggered", fun() {
      this.undo();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  action = qt.QAction.new("Redo", this);
  action.setShortcut("ctrl+shift+=");
  action.connect("triggered", fun() {
      this.redo();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  // exploring actions

  var action = qt.QAction.new("Do it", this);
  action.setShortcut("ctrl+d");
  action.connect("triggered", fun() {
      this.doIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);

  var action = qt.QAction.new("Spawn and Do it", this);
  action.setShortcut("ctrl+shift+d");
  action.connect("triggered", fun() {
      this.spawnAndDoIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);

  action = qt.QAction.new("Print it", this);
  action.setShortcut("ctrl+p");
  action.connect("triggered", fun() {
      this.printIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);

  action = qt.QAction.new("Inspect it", this);
  action.setShortcut("ctrl+i");
  action.connect("triggered", fun() {
      this.inspectIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);

  action = qt.QAction.new("Debug it", this);
  action.setShortcut("ctrl+b");
  action.connect("triggered", fun() {
      this.debugIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);


  var action = qt.QAction.new("Spawn and Debug it", this);
  action.setShortcut("ctrl+shift+b");
  action.connect("triggered", fun() {
      this.spawnAndDebugIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);
}
//end idez:Editor:initActions

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

end //idez:Editor

class ExecutionFrames
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

instance_method frame: fun(i) {
  return @vmproc.stackFrames().get(i);
}

instance_method locationInfoFor: fun(i) {
  return @vmproc.stackFrames().get(i).instructionPointer();
}

instance_method names: fun() {
  return @vmproc.stackFrames().map(fun(frame) {
    frame.contextPointer.compiledFunction.fullName + ":" + frame.instructionPointer["start_line"].toString
  });
}

instance_method size: fun() {
  return @vmproc.stackFrames().size();
}

instance_method topFrame: fun() {
  return @vmproc.stackFrames().get(this.size()-1);
}

end //idez:ExecutionFrames

class ExplorerEditor < MemeEditor
fields: cfun;
init new: fun(parent, cfun) {
  super.new(parent);
  this.setStyleSheet("border-style: outset;");
  @cfun = cfun;
  if (cfun != null) {
    this.setText(cfun.text);
  }
}

instance_method cfun: fun() {
  return @cfun;
}

instance_method accept: fun() {
  if (@cfun != null) {
    try {
      @cfun.setCode(this.text());
      this.saved();
    } catch(ex) {
      this.appendSelectedText(ex.message());
    }
  }
}

end //idez:ExplorerEditor

class Inspector < QMainWindow
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

  @textArea = MemeEditor.new(centralWidget);
  @textArea.withVariables(fun() { {"this" : @inspectee} });
  hbox.addWidget(@textArea);

  mainLayout.addLayout(hbox);

  @lineEdit = LineEditor.new(centralWidget, @inspectee, true);
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

  var execMenu = this.menuBar().addMenu("Explore");

  var action = qt.QAction.new("Do it", this);
  action.setShortcut("ctrl+d");
  action.connect("triggered", fun() {
      this.doIt();
  });
  execMenu.addAction(action);
  action.setShortcutContext(0); //widget context

  action = qt.QAction.new("Print it", this);
  action.setShortcut("ctrl+p");
  action.connect("triggered", fun() {
      this.printIt();
  });
  execMenu.addAction(action);
  action.setShortcutContext(0); //widget context

  action = qt.QAction.new("Inspect it", this);
  action.setShortcut("ctrl+i");
  action.connect("triggered", fun() {
      this.inspectIt();
  });
  execMenu.addAction(action);
  action.setShortcutContext(0); //widget context

  action = qt.QAction.new("Debug it", this);
  action.setShortcut("ctrl+b");
  action.connect("triggered", fun() {
      this.debugIt();
  });
  execMenu.addAction(action);
  action.setShortcutContext(0); //widget context

  action = qt.QAction.new("Accept It", execMenu);
  action.setShortcut("ctrl+x,a");
  action.connect("triggered", fun() {
      this.acceptIt();
  });
  execMenu.addAction(action);
}
//end idez:Inspector:new

instance_method acceptIt: fun() {
  try {
    //thisModule here: not appropriate.
    //it should be the module where inspectee was defined
    // -- perhaps, inspectee's vt if it is a class.
    // [if it isn't, fuck, use kernel imodule.]
    var ctx = Context.withVars(@textArea.text(), {"this":@inspectee}, thisModule);
    var new_value = ctx();
    var slot = @fieldList.currentItem().text();
    @mirror.setValueFor(slot, new_value);
    @textArea.saved();
  } catch(ex) {
    @textArea.appendSelectedText(ex.message());
  }
  // if (@target_process) {
  //   @target_process.updateObject(@inspectee);
  // }
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

class_method inspect: fun(target) {
  return Inspector.new(target).show();
}

end //idez:Inspector

class LineEditor < QLineEdit
fields: receiver;
init new: fun(parent, receiver, returnDoesIt) {
  super.new(parent);
  this.initActions(returnDoesIt);
  @receiver = receiver;
}

instance_method doIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), {"this":@receiver}, thisModule);
    ctx();
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method initActions: fun(bindReturn) {
  if (bindReturn) {
    this.connect("returnPressed", fun() {
      this.selectAllAndDoit(null);
    });
  }

  var action = qt.QAction.new("Do it", this);
  action.setShortcut("ctrl+d");
  action.connect("triggered", fun() {
      this.doIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);

  action = qt.QAction.new("Print it", this);
  action.setShortcut("ctrl+p");
  action.connect("triggered", fun() {
      this.printIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);

  action = qt.QAction.new("Inspect it", this);
  action.setShortcut("ctrl+i");
  action.connect("triggered", fun() {
      this.inspectIt();
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
    var ctx = Context.withVars(this.selectedText(), {"this":@receiver}, thisModule);
    var res = ctx();
    Inspector.inspect(res);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method printIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), {"this":@receiver}, thisModule);
    var res = ctx();
    this.insertSelectedText(res.toString());
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method selectAllAndDoit: fun(receiver) {
  this.selectAll();
  this.doIt();
}

end //idez:LineEditor

class MemeEditor < Editor
fields: with_variables, with_imod, on_finish;
init new: fun(parent) {
  super.new(parent);

  @with_variables = fun () { {} };
  @with_imod = fun() { thisModule };
  @on_finish = fun(x) { };
}

instance_method debugIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), @with_variables(), @with_imod());
    VMProcess.current.haltFn(ctx, []);
    @on_finish(ctx.getEnv());
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method doIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), @with_variables(), @with_imod());
    ctx();
    @on_finish(ctx.getEnv());
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method inspectIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), @with_variables(), @with_imod());
    var res = ctx();
    @on_finish(ctx.getEnv());
    Inspector.inspect(res);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method onFinish: fun(fn) {
  @on_finish = fn;
}

instance_method printIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), @with_variables(), @with_imod());
    var res = ctx();
    @on_finish(ctx.getEnv());
    this.insertSelectedText(res.toString());
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method spawnAndDoIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), @with_variables(), @with_imod());
    var proc = VMProcess.spawn();
    proc.debugOnException();
    proc.run(ctx, []);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method spawnAndDebugIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), @with_variables(), @with_imod());
    var proc = VMProcess.spawn();
    proc.runAndHalt(ctx, []);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method withIModule: fun(fn) {
  @with_imod = fn;
}

instance_method withVariables: fun(fn) {
  @with_variables = fn;
}

end //idez:MemeEditor

class MiniBuffer < QWidget
fields: label, lineEdit, callback;
init new: fun(parent) {
  super.new(parent);

  this.hide();
  this.setMaximumHeight(30);
  @callback = null;
  @lineEdit = LineEditor.new(this, thisModule, false);
  @lineEdit.setMinimumSize(200,30);
  @label = qt.QLabel.new(this);
  var l = qt.QHBoxLayout.new(this);
  l.addWidget(@label);
  l.addWidget(@lineEdit);
  l.setContentsMargins(10,2,10,2);

  @lineEdit.connect("returnPressed", fun() {
    if (@callback) {
      var close = @callback(@lineEdit.text());
      if (close != false) { this.hide(); }
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

end //idez:MiniBuffer

class ModuleExplorer < QMainWindow
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
  @statusLabel.setMinimumWidth(800);

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
        e =ExplorerEditor.new(null, cfn);
        e.withVariables(fun() { @variables });
        e.withIModule(fun() { this.currentIModule() });
        e.onFinish(fun(env) { @variables = env + @variables;});
      }
    } else {
      if (params.has("code")) {
        e = ExplorerEditor.new(null, null);
        e.withVariables(fun() { @variables });
        e.withIModule(fun() { this.currentIModule() });
        e.onFinish(fun(env) { @variables = env + @variables;});
        e.setText(params["code"]);
      }
    }
    return e;
  });

  @webview.connect('linkClicked', fun(url) {
    io.print("URL selected: " + url.toString());

    if (url.path() == "/") {
      this.show_home();
      return null;
    }

    if (url.path() == "/tutorial") {
      this.show_tutorial();
      return null;
    }
    if (url.path() == "/modules-index") {
      this.show_modules();
      return null;
    }
    if (url.path() == "/module") {
      var name = url.queryItemValue("name");
      @current_cmodule = get_module(name);
      this.show_module(name);
      return null;
    }

    if (url.path() == "/class") {
      var module_name = url.queryItemValue("module");
      var class_name = url.queryItemValue("class");
      @current_cmodule = get_module(module_name);
      this.show_class(module_name, class_name);
      return null;
    }

    if (url.path() == "/show-editor") {
      if (url.hasQueryItem("class")) {
        this.show_editor(url.queryItemValue("id"), url.queryItemValue("name"), url.queryItemValue("type"), url.queryItemValue("class"));
      } else {
        this.show_editor(url.queryItemValue("id"), url.queryItemValue("name"), url.queryItemValue("type"), null);
      }
    }

    if (url.hasFragment()) {
      @webview.page().mainFrame().scrollToAnchor(url.fragment());
      return null;
    }
  });

  this.initActions();
  this.show_home();
}
//end idez:ModuleExplorer:new

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
      this.show_function(cfun, "module", ".module-functions");
      mlist.appendInside("<li><a href='#" + cfun.fullName + "'>" + cfun.fullName + "</a></li>");
      @statusLabel.setText("Function added: " + name);
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
      this.show_function(cfun, flag.toString, "div[id='imethods_" + klass.name + "']");
      mlist.appendInside("<li><a href='#" + cfun.fullName + "'>" + cfun.fullName + "</a></li>");
      @statusLabel.setText("Method added: " + cfun.fullName);
    }, fun() {
      mlist.findFirst("li a[href='#" + cfun.fullName + "']").setAttribute("style","display:none");
      klass.removeMethod(method_name, flag);
      doc.findFirst("div[id='" + cfun.fullName + "']").takeFromDocument();
      @statusLabel.setText("Method removed: " + cfun.fullName);
    });
  });

}
//end idez:ModuleExplorer:action_addMethod

instance_method action_debug: fun() {
  @miniBuffer.prompt("Debug: ", "", fun(expr) {
    try {
      var imod = null;
      if (@imodule) {
        imod = @imodule;
      } else {
        imod = thisModule;
      }

      var ctx = Context.withVars(expr, {}, imod);
      VMProcess.current.haltFn(ctx, []);
    } catch(e) {
      @statusLabel.setText(e.message);
    }
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
      @statusLabel.setText("Class deleted: " + name);
    }, fun() {
      @current_cmodule.addClass(klass);
      this.show_module(@current_cmodule.name);
      @statusLabel.setText("Class added: " + name);
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
        @statusLabel.setText("Fields accepted: " + fields);
      }, fun() {
        klass.setFields(old_fields);
        doc.findFirst("div[id='" + klass.fullName + "'] .fields_list").setPlainText(old_fields.toString());
        @statusLabel.setText("Fields accepted: " + old_fields.toString);
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
        @statusLabel.setText("Default parameter for '" + name + "' now is: " + val);
      }, fun() {
        @current_cmodule.setDefaultParameter(name, dp);
        var lis = @current_cmodule.default_parameters.map(fun(name,d) { "<li>" + name + " : memetalk/" + d["value"] + "/1.0</li>"; });
        doc.findFirst(".default-parameters").setInnerXml(lis.join(""));
        @statusLabel.setText("Default parameter for '" + name + "' now is: " + dp.toString);
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
        @statusLabel.setText("Module parameters: " + params);
    }, fun() {
      @current_cmodule.setParams(old_params);
      doc.findFirst(".module_title_params").setPlainText(old_params.toString);
      @statusLabel.setText("Module parameters: " + old_params.toString);
    });
  });
}

instance_method action_eval: fun() {
  @miniBuffer.prompt("Eval: ", "", fun(expr) {
    try {
      var imod = null;
      if (@imodule) {
        imod = @imodule;
      } else {
        imod = thisModule;
      }
      var ctx = Context.withVars(expr, {}, imod);
      var r = ctx();
      @statusLabel.setText(r.toString());
    } catch(e) {
      @statusLabel.setText(e.message);
    }
  });
}

instance_method action_spawnAndEval: fun() {
  @miniBuffer.prompt("Eval [spawn]: ", "", fun(expr) {
    try {
      var imod = null;
      if (@imodule) {
        imod = @imodule;
      } else {
        imod = thisModule;
      }
      var ctx = Context.withVars(expr, {}, imod);
      var proc = VMProcess.spawn();
      proc.debugOnException();
      proc.run(ctx, []);
    } catch(e) {
      @statusLabel.setText(e.message);
    }
  });
}

instance_method action_spawnAndDebug: fun() {
  @miniBuffer.prompt("Debug [spawn]: ", "", fun(expr) {
    try {
      var imod = null;
      if (@imodule) {
        imod = @imodule;
      } else {
        imod = thisModule;
      }
      var ctx = Context.withVars(expr, {}, imod);
      var proc = VMProcess.spawn();
      proc.debugOnException();
      proc.runAndHalt(ctx, []);
    } catch(e) {
      @statusLabel.setText(e.message);
    }
  });
}

instance_method action_spawnAndEvalUntil: fun() {
  if (@current_cmodule == null) {
    @statusLabel.setText("No current module");
    return null;
  }
  var e = qt.QApplication.focusWidget();
  if (Mirror.vtFor(e) != ExplorerEditor) {
    @statusLabel.setText("No function selected");
    return null;
  }
  @miniBuffer.prompt("Spawn/Eval [until]: ", "", fun(expr) {
    try {
      var imod = null;
      if (@imodule) {
        imod = @imodule;
      } else {
        imod = thisModule;
      }

      var ctx = Context.withVars(expr, {}, imod);
      var proc = VMProcess.spawn();
      proc.debugOnException();
      proc.breakAt(e.cfun, e.getCursorPosition["line"] + 1);
      proc.run(ctx, []);
    } catch(ex) {
      @statusLabel.setText(ex.message);
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
  @miniBuffer.prompt("Eval [until]: ", "", fun(expr) {
    try {
      var imod = null;
      if (@imodule) {
        imod = @imodule;
      } else {
        imod = thisModule;
      }

      VMProcess.current.breakAt(e.cfun, e.getCursorPosition["line"] + 1);
      var ctx = Context.withVars(expr, {}, imod);
      var r = ctx();
      @statusLabel.setText(r.toString());
    } catch(ex) {
      @statusLabel.setText(ex.message);
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
  @statusLabel.setText("System saved");
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

  var action = qt.QAction.new("Inspect Window", this);
  action.setShortcut("alt+w,i");
  action.connect("triggered", fun() {
    Inspector.new(this).show()
  });
  action.setShortcutContext(1);
  this.addAction(action);

  // System menu
  var execMenu = this.menuBar().addMenu("System");
  @sysmenu = execMenu;
  action = qt.QAction.new("Save", execMenu);
  action.setShortcut("ctrl+x,s");
  action.connect("triggered", fun() {
    this.action_saveToFileSystem();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Accept code", execMenu);
  action.setShortcut("ctrl+x,a");
  action.connect("triggered", fun() {
    this.action_acceptCode();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Eval", execMenu);
  action.setShortcut("ctrl+x,e");
  action.connect("triggered", fun() {
    this.action_eval();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Spawn and Eval", execMenu);
  action.setShortcut("alt+x,e");
  action.connect("triggered", fun() {
    this.action_spawnAndEval();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Eval Until", execMenu);
  action.setShortcut("ctrl+x,u");
  action.connect("triggered", fun() {
    this.action_evalUntil();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Spawn and Eval Until", execMenu);
  action.setShortcut("alt+x,u");
  action.connect("triggered", fun() {
    this.action_spawnAndEvalUntil();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Debug", execMenu);
  action.setShortcut("ctrl+x,b");
  action.connect("triggered", fun() {
    this.action_debug();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Spawn and Debug", execMenu);
  action.setShortcut("alt+x,b");
  action.connect("triggered", fun() {
    this.action_spawnAndDebug();
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
  action.setShortcut("alt+z");
  action.connect("triggered", fun() {
    @chistory.undo();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Redo", execMenu);
  action.setShortcut("alt+y");
  action.connect("triggered", fun() {
    @chistory.redo();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Reset", execMenu);
  action.setShortcut("ctrl+r");
  action.connect("triggered", fun() {
    this.action_reset()
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  // Module Menu

  execMenu = this.menuBar().addMenu("Module");

  action = qt.QAction.new("Instantiate module", execMenu);
  action.setShortcut("ctrl+m,i");
  action.connect("triggered", fun() {
    this.action_instantiateModule();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Rename module", execMenu);
  action.setShortcut("ctrl+m,r");
  action.connect("triggered", fun() {
    io.print("rename module");
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Edit Module Parameters", execMenu);
  action.setShortcut("ctrl+m,p");
  action.connect("triggered", fun() {
    this.action_editModuleParameters();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Edit Module Default Parameters", execMenu);
  action.setShortcut("ctrl+m,d");
  action.connect("triggered", fun() {
    this.action_editModuleDefaultParameters();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("New Module Function", execMenu);
  action.setShortcut("ctrl+m,f");
  action.connect("triggered", fun() {
    this.action_addFunction()
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  var del_fn_ac = qt.QAction.new("Delete Function/Method", execMenu);
  del_fn_ac.setShortcut("ctrl+f,k");
  del_fn_ac.connect("triggered", fun() {
    this.action_deleteFunction()
  });
  del_fn_ac.setShortcutContext(1);
  execMenu.addAction(del_fn_ac);

  // Class Menu

  execMenu = this.menuBar().addMenu("Class");
  action = qt.QAction.new("New Class", execMenu);
  action.setShortcut("ctrl+c,n");
  action.connect("triggered", fun() {
    this.action_addClass();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Delete Class", execMenu);
  action.setShortcut("ctrl+c,k");
  action.connect("triggered", fun() {
    this.action_deleteClass();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Rename Class", execMenu);
  action.setShortcut("ctrl+c,r");
  action.connect("triggered", fun() {
    this.action_renameClass();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("Edit Fields", execMenu);
  action.setShortcut("ctrl+c,f");
  action.connect("triggered", fun() {
    this.action_editFields();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  action = qt.QAction.new("New Method", execMenu);
  action.setShortcut("ctrl+c,m");
  action.connect("triggered", fun() {
    this.action_addMethod();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);

  execMenu.addAction(del_fn_ac);

  action = qt.QAction.new("Toggle Method as Constructor", execMenu);
  action.setShortcut("ctrl+c,c");
  action.connect("triggered", fun() {
    this.action_toggleCtor();
  });
  action.setShortcutContext(1);
  execMenu.addAction(action);
}
//end idez:ModuleExplorer:initActions

instance_method show_class: fun(module_name, class_name) {
  this.show_module_basic(module_name);
  var doc = @webview.page().mainFrame().documentElement();
  var mlist = doc.findFirst("#menu-listing .link-list");

  var klass = @current_cmodule.compiled_classes[class_name];

  var div = doc.findFirst("div[id=templates] .class_template").clone();
  doc.findFirst(".module-classes").appendInside(div);
  div.setStyleProperty("display","block");
  div.setAttribute("id", klass.fullName);

  div.findFirst(".class_name").setPlainText(klass.name);
  div.findFirst(".super_class").setPlainText(klass.super_class_name);

  div.findFirst(".fields_list").setPlainText(klass.fields.toString());

  var ctors = div.findFirst(".constructors");
  ctors.setAttribute("id", "ctors_" + klass.name);

  klass.constructors.sortedEach(fun(name, cfn) {
    mlist.appendInside("<li><a href='#" + cfn.fullName + "'>*" + cfn.fullName + "</a></li>");
    this.show_function(cfn, "ctor_method", "div[id='ctors_" + klass.name + "']");
  });
  var ims = div.findFirst(".instance_methods");
  ims.setAttribute("id", "imethods_" + klass.name);

  klass.instanceMethods.sortedEach(fun(name, cfn) {
   mlist.appendInside("<li><a href='#" + cfn.fullName + "'>" + cfn.fullName + "</a></li>");
   this.show_function(cfn, "instance_method", "div[id='imethods_" + klass.name + "']");
  });

  var cms = div.findFirst(".class_methods");
  cms.setAttribute("id", "cmethods_" + klass.name);

  klass.classMethods.sortedEach(fun(name, cfn) {
    mlist.appendInside("<li><a href='#" + cfn.fullName + "'>[" + cfn.fullName + "]</a></li>");
    this.show_function(cfn, "class_method", "div[id='cmethods_" + klass.name + "']");
  });
}

instance_method show_editor: fun(id, name, type, class_name) {
  var doc = @webview.page().mainFrame().documentElement();
  var div = doc.findFirst("div[id='" + id + "']");
  var cfn = null;

  div.findFirst(".method-click-advice").takeFromDocument();

  var divcode = div.findFirst(".function-source-code");
  divcode.appendInside("<object width=800></object>");
  var obj = divcode.findFirst("object");
  obj.takeFromDocument;
  obj.setInnerXml("<param name='class_name'/><param name='function_type'/><param name='module_name'/><param name='function_name'/><param name='code'/>");

  if (type == "module") {
    cfn = @current_cmodule.compiled_functions[name];
  }
  if (type == "ctor_method") {
    cfn = @current_cmodule.compiled_classes[class_name].constructors[name];
    obj.findFirst("param[name='class_name']").setAttribute("value",cfn.owner.name);
  }
  if (type == "class_method") {
    cfn = @current_cmodule.compiled_classes[class_name].classMethods[name];
    obj.findFirst("param[name='class_name']").setAttribute("value",cfn.owner.name);
  }
  if (type == "instance_method") {
    cfn = @current_cmodule.compiled_classes[class_name].instanceMethods[name];
    obj.findFirst("param[name='class_name']").setAttribute("value",cfn.owner.name);
  }

  obj.findFirst("param[name='function_type']").setAttribute("value",type);
  obj.findFirst("param[name='module_name']").setAttribute("value",@current_cmodule.name);
  obj.findFirst("param[name='function_name']").setAttribute("value",cfn.name);
  obj.findFirst("param[name='code']").setAttribute("value",cfn.text);
  obj.setAttribute("type","x-pyqt/editor");
  divcode.appendInside(obj);
}

instance_method show_function: fun(cfn, function_type, parent_sel) {
  var doc = @webview.page().mainFrame().documentElement();
  var parent = doc.findFirst(parent_sel);
  var div = doc.findFirst("div[id=templates] .function_template").clone();
  div.setAttribute("id", cfn.fullName);
  div.setStyleProperty("display","block");
  div.findFirst(".function_name").setPlainText(cfn.fullName);
  div.findFirst(".function_name").setAttribute("name",cfn.fullName);

  if (Mirror.vtFor(cfn.owner) == CompiledClass) {
    div.findFirst(".method-click-advice").setAttribute("href","/show-editor?class=" + cfn.owner.name + "&name=" + cfn.name + "&type=" + function_type + "&id=" + cfn.fullName);
  } else {
    div.findFirst(".method-click-advice").setAttribute("href","/show-editor?name=" + cfn.name  + "&id=" + cfn.fullName + "&type=" + function_type);
  }
  parent.appendInside(div);
}

instance_method show_home: fun() {
  @current_cmodule = null;
  @imodule = null;
  @webview.setUrl(modules_path() + "/module-explorer/index.html");
}

instance_method show_module: fun(name) {
  this.show_module_basic(name);

  var doc = @webview.page().mainFrame().documentElement();
  var mlist = doc.findFirst("#menu-listing .link-list");
  var fns = @current_cmodule.compiled_functions();
  fns.sortedEach(fun(name,cfn) {
    mlist.appendInside("<li><a href='#" + cfn.fullName + "'>" + cfn.fullName + "</a></li>");
    this.show_function(cfn, "module", ".module-functions");
  });

  doc.findFirst(".module-functions").setAttribute("style","display:block");
  @current_cmodule.compiled_classes().sortedEach(fun(name, klass) {
    mlist.appendInside("<li><a href='/class?module=" + klass.module.name + "&class=" + klass.name + "'>" + klass.fullName + "</a></li>");
  });
}

instance_method show_module_basic: fun(name) {
  @webview.loadUrl(modules_path() + "/module-explorer/module-view.html");
  var doc = @webview.page().mainFrame().documentElement();
  doc.findFirst("head").appendInside('<link rel="stylesheet" href="file://' + modules_path() + '/module-explorer/style.css" type="text/css">');

  var name = @current_cmodule.name;
  doc.findFirst("#module_title").setInnerXml("<a href='/module?name=" + name + "'>" + name + "</a>");

  doc.findFirst(".module_title_params").setPlainText(@current_cmodule.params.toString());

  var dp = @current_cmodule.default_parameters.map(fun(name,d) { "<li>" + name + " : memetalk/" + d["value"] + "/1.0</li>"; });
  doc.findFirst(".default-parameters").appendInside(dp.join(""));
}

instance_method show_modules: fun() {
  @current_cmodule = null;
  @imodule = null;
  @webview.loadUrl(modules_path() + "/module-explorer/modules-index.html");
  var modules = available_modules();
  var ul = @webview.page().mainFrame().documentElement().findFirst("ul.modules");
  modules.each(fun(name) {
    ul.appendInside("<li><a href='/module?name=" + name + "'>" + name + "</a></li>")
  });
}

instance_method show_tutorial: fun() {
  @current_cmodule = null;
  @imodule = null;
  @webview.setUrl(modules_path() + "/module-explorer/tutorial.html");
}

end //idez:ModuleExplorer

class StackCombo < QComboBox
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

end //idez:StackCombo

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
    Inspector.inspect(item.object)
  });
}

instance_method loadFrame: fun(frame) {
  this.clear();
  this.setHorizontalHeaderLabels(['Name', 'Value']);

  if (frame.environmentPointer) {
    var env = frame.environmentPointer;
    var table = frame.contextPointer.compiledFunction.env_table;

    this.setRowCount(env.size);

    var _this = env['r_rp'];
    this.setItem(0, 0, VariableItem.new("this", _this));
    this.setItem(0, 1, VariableItem.new(_this.toSource, _this));

    var _dthis = env['r_rdp'];
    this.setItem(1, 0, VariableItem.new("@this", _dthis));
    this.setItem(1, 1, VariableItem.new(_dthis.toSource, _dthis));

    var i = 2;
    table.each(fun(idx, varname) {
      var entry = env[idx];
      this.setItem(i, 0, VariableItem.new(varname, entry));
      this.setItem(i, 1, VariableItem.new(entry.toSource, entry));
      i = i + 1;
    });
  } else {
    this.setRowCount(2 + frame.locals.size);

    var _this = frame.receiverPointer;
    this.setItem(0, 0, VariableItem.new("this", _this));
    this.setItem(0, 1, VariableItem.new(_this.toSource, _this));

    var _dthis = frame.receiverDataPointer;
    this.setItem(1, 0, VariableItem.new("@this", _dthis));
    this.setItem(1, 1, VariableItem.new(_dthis.toSource, _dthis));

    var i = 2;
    frame.locals.each(fun(name,val) {
      this.setItem(i, 0, VariableItem.new(name, frame.locals[name]));
      this.setItem(i, 1, VariableItem.new(val.toSource, frame.locals[name]));
      i = i + 1;
    });
  }
}
//end idez:VariableListWidget:loadFrame

instance_method loadReceiver: fun(frame) {
  this.clear();
  this.setHorizontalHeaderLabels(['Name', 'Value']);

  var _dthis = null;
  if (frame.environmentPointer) {
    _dthis = frame.environmentPointer['r_rdp'];
  } else {
    _dthis = frame.receiverDataPointer;
  }
  var mirror = Mirror.new(_dthis);
  var fields = mirror.fields();
  this.setRowCount(fields.size);

  var i = 0;
  fields.each(fun(name) {
    this.setItem(i, 0, VariableItem.new(name, mirror.valueFor(name)));
    this.setItem(i, 1, VariableItem.new(mirror.valueFor(name).toString, mirror.valueFor(name)));
    i = i + 1;
  });
}

end //idez:VariableListWidget

class Workspace < QMainWindow
fields: editor, variables;
init new: fun() {
  super.new();
  @variables = {};

  this.setWindowTitle("Workspace");

  @editor = MemeEditor.new(this);
  @editor.withVariables(fun() { @variables });
  @editor.onFinish(fun(env) { @variables = env + @variables; });

  this.setCentralWidget(@editor);

  var execMenu = this.menuBar().addMenu("Edit");
  @editor.editingActions.each(fun(ac) {
    execMenu.addAction(ac);
  });
  var execMenu = this.menuBar().addMenu("Explore");
  @editor.exploringActions.each(fun(ac) {
    execMenu.addAction(ac);
  });
}

end //idez:Workspace


.end
