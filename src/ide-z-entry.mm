module main() {

  fun import(mname, margs) {
    <primitive "import">
  }

  fun main() {
    var qt = import("qt.mm", []);
    var idez = import("ide-z.mm", [qt,
                                   qt.QMainWindow,
                                   qt.QPlainTextEdit]);
    idez.main();
  }
}
