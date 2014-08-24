.preamble(qt, io)
  qt : meme:qt;
  io : meme:io;
  [QMainWindow, QsciScintilla, QLineEdit, QWidget] <= qt;
.code

class LineEditor < QLineEdit
fields: receiver;
init new: fun(parent, receiver, returnDoesIt) {
  super.new(parent);
  this.initActions(returnDoesIt);
  @receiver = receiver;
}

instance_method doIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), {:this:@receiver}, thisModule);
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
  action.connect("triggered", fun(_) {
      this.doIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);

  action = qt.QAction.new("Print it", this);
  action.setShortcut("ctrl+p");
  action.connect("triggered", fun(_) {
      this.printIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);

  action = qt.QAction.new("Inspect it", this);
  action.setShortcut("ctrl+i");
  action.connect("triggered", fun(_) {
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
    var ctx = Context.withVars(this.selectedText(), {:this:@receiver}, thisModule);
    var res = ctx();
    Inspector.inspect(res);
  } catch(ex) {
    this.insertSelectedText(ex.message());
  }
}

instance_method printIt: fun() {
  try {
    var ctx = Context.withVars(this.selectedText(), {:this:@receiver}, thisModule);
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
  action.connect("triggered", fun(_) {
      this.cut();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  action = qt.QAction.new("Copy", this);
  action.setShortcut("alt+w");
  action.connect("triggered", fun(_) {
      this.copy();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  action = qt.QAction.new("Paste", this);
  action.setShortcut("ctrl+y");
  action.connect("triggered", fun(_) {
      this.paste();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  action = qt.QAction.new("Undo", this);
  action.setShortcut("ctrl+shift+-");
  action.connect("triggered", fun(_) {
      this.undo();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  action = qt.QAction.new("Redo", this);
  action.setShortcut("ctrl+shift+=");
  action.connect("triggered", fun(_) {
      this.redo();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @editing_actions.append(action);

  // exploring actions

  action = qt.QAction.new("Do it", this);
  action.setShortcut("ctrl+d");
  action.connect("triggered", fun(_) {
      this.doIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);

  action = qt.QAction.new("Spawn and Do it", this);
  action.setShortcut("ctrl+shift+d");
  action.connect("triggered", fun(_) {
      this.spawnAndDoIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);

  action = qt.QAction.new("Print it", this);
  action.setShortcut("ctrl+p");
  action.connect("triggered", fun(_) {
      this.printIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);

  action = qt.QAction.new("Inspect it", this);
  action.setShortcut("ctrl+i");
  action.connect("triggered", fun(_) {
      this.inspectIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);

  action = qt.QAction.new("Debug it", this);
  action.setShortcut("ctrl+b");
  action.connect("triggered", fun(_) {
      this.debugIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);


  action = qt.QAction.new("Spawn and Debug it", this);
  action.setShortcut("ctrl+shift+b");
  action.connect("triggered", fun(_) {
      this.spawnAndDebugIt();
  });
  action.setShortcutContext(0); //widget context
  this.addAction(action);
  @exploring_actions.append(action);
}
//end idez:Editor:initActions

instance_method insertSelectedText: fun(text) {
  var pos = this.getSelection();
  this.insertAt(text, pos[:end_line], pos[:end_index]);
  var nl = text.count("\n");
  if (nl == 0) {
    this.setSelection(pos[:end_line], pos[:end_index], pos[:end_line], pos[:end_index] + text.size);
  } else {
    var pl = text.rindex("\n");
    this.setSelection(pos[:end_line], pos[:end_index], pos[:end_line] + nl, text.from(pl).size() - 1);
  }
}

end //idez:Editor

class MemeEditor < Editor
fields: with_variables, with_imod, on_finish;
init new: fun(parent) {
  super.new(parent);

  @with_variables = fun () { {} };
  @with_imod = fun() { thisModule };
  @on_finish = fun(x) { };
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


instance_method withIModule: fun(fn) {
  @with_imod = fn;
}

instance_method withVariables: fun(fn) {
  @with_variables = fn;
}

end //idez:MemeEditor

class Workspace < QMainWindow
  fields: editor, variables;
  init new: fun() {
    super.new(null);
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
    execMenu = this.menuBar().addMenu("Explore");
    @editor.exploringActions.each(fun(ac) {
      execMenu.addAction(ac);
    });
  }
end //idez:Workspace


////////////////////////

class Inspector < QMainWindow
fields: inspectee, variables, mirror, fieldList, textArea, lineEdit;
init new: fun(inspectee) {
  super.new(null);

  @variables = {:this:@inspectee};
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
  @textArea.withVariables(fun() { {:this : @inspectee} });
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
    this.itemSelected(item)
  });
  @fieldList.connect("itemActivated", fun(item) {
      this.itemActivated(item);
  });

  var execMenu = this.menuBar().addMenu("Explore");

  var action = qt.QAction.new("Do it", this);
  action.setShortcut("ctrl+d");
  action.connect("triggered", fun(_) {
      this.doIt();
  });
  execMenu.addAction(action);
  action.setShortcutContext(0); //widget context

  action = qt.QAction.new("Print it", this);
  action.setShortcut("ctrl+p");
  action.connect("triggered", fun(_) {
      this.printIt();
  });
  execMenu.addAction(action);
  action.setShortcutContext(0); //widget context

  action = qt.QAction.new("Inspect it", this);
  action.setShortcut("ctrl+i");
  action.connect("triggered", fun(_) {
      this.inspectIt();
  });
  execMenu.addAction(action);
  action.setShortcutContext(0); //widget context

  action = qt.QAction.new("Debug it", this);
  action.setShortcut("ctrl+b");
  action.connect("triggered", fun(_) {
      this.debugIt();
  });
  execMenu.addAction(action);
  action.setShortcutContext(0); //widget context

  action = qt.QAction.new("Accept It", execMenu);
  action.setShortcut("ctrl+x,a");
  action.connect("triggered", fun(_) {
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
    var ctx = Context.withVars(@textArea.text(), {:this:@inspectee}, thisModule);
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

main: fun() {
  var app = qt.QApplication.new();
  var w = Workspace.new;
  w.show();
  app.exec();
}

.endcode
