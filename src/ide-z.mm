module idez(qt, QWidget, QMainWindow, QPlainTextEdit) {

  fun getClass_CompiledFunction() {
    <primitive "class_compiled_function">
  }

  fun get_current_compiled_module() {
    <primitive "get_current_compiled_module">
  }

  fun print(arg) {
    <primitive "print">
  }

  class Editor < QPlainTextEdit {
    fields: variables;
    init new(parent) {
      super.new(parent);
      @variables = {};
    }

    fun doIt() {
      var selection = this.selectedText();

      var CompiledFunction = getClass_CompiledFunction();
      var cmod = get_current_compiled_module();
      var cfun = CompiledFunction.new(selection, [], cmod, @variables);
      var fn = cfun.asContext(thisModule, @variables);
      var res =  fn.apply([]);
      @variables = fn.getEnv() + @variables;
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
      @editor.setTabStopWidth(20);
      this.setCentralWidget(@editor);
      this.initActions();
    }
    fun initActions() {
      var execMenu = this.menuBar().addMenu("&Execution");
      var action = qt.QAction.new("&Do it", execMenu);
      action.setShortcut("ctrl+d");
      action.connect("triggered", fun() {
          @editor.doIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("&Print it", execMenu);
      action.setShortcut("ctrl+p");
      action.connect("triggered", fun() {
          @editor.printIt();
      });
      execMenu.addAction(action);

      action = qt.QAction.new("&Inspect it", execMenu);
      action.setShortcut("ctrl+i");
      action.connect("triggered", fun() {
          @editor.inspectIt();
      });
      execMenu.addAction(action);
    }
  }

  class Inspector  < QMainWindow {
    fields: inspectee, fieldList, textArea, lineEdit;
    init new(inspectee) {
      super.new();
      @inspectee = inspectee;

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
      mainLayout.addWidget(@lineEdit);

      this.setCentralWidget(centralWidget);
    }
  }

  fun main() {
    var app = qt.QApplication.new();
    var main = Workspace.new();
    main.resize(300,300);
    main.show();
    app.exec();
  }
}
