module main() {

  fun import(mname, margs) {
    <primitive "import">
  }

  fun main() {
    var qt = import("qt.mm", []);
    var idez = import("ide-z.mm", [qt,
                                   qt.QWidget, //super classes go explict
                                   qt.QMainWindow,
                                   qt.QPlainTextEdit]);
    idez.main();
  }
}
