module idez(qt, QWidget, QMainWindow, QPlainTextEdit) {

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
}
