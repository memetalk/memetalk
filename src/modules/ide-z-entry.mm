module idez_entry() {

  fun import(mname, margs) {
    <primitive "import">
  }

  fun main() {
    var qt = import("qt.mm", []);
    var idez = import("ide-z.mm", [qt,
                                   qt.QWidget, //super classes go explict
                                   qt.QMainWindow,
                                   qt.QPlainTextEdit,
                                   qt.QComboBox,
                                   qt.QTableWidget]);
    idez.main();
  }

  fun debug(process) {
    var qt = import("qt.mm", []);
    var idez = import("ide-z.mm", [qt,
                                   qt.QWidget, //super classes go explict
                                   qt.QMainWindow,
                                   qt.QPlainTextEdit,
                                   qt.QComboBox,
                                   qt.QTableWidget]);
    idez.debug(process);
  }
}
