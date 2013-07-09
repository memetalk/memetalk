module idez_entry() {

  fun import(mname, margs) {
    <primitive "import">
  }

  fun main() {
    var qt = import("qt", []);
    var idez = import("ide-z", [qt,
                                   qt.QWidget, //super classes go explict
                                   qt.QMainWindow,
                                   qt.QPlainTextEdit]);
    idez.main();
  }
}
