module idez(qt, QMainWindow, QPlainTextEdit) {

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
    fun selectedText() {
      return this.textCursor().selectedText();
    }
    fun printIt() {
      var res = this.doIt();
      this.insertSelectedText(res.toString());
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
