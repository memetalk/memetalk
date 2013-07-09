module debugger_entry() {

  fun import(mname, margs) {
    <primitive "import">
  }

  fun main(proc) {
    //TODO: set those right: see debugger.mm module params

    var qt = import("qt.mm", []);
    var dbg = import("debugger.mm", [qt.QWidget,
                                     qt.QComboBox,
                                     qt.QTableWidget,
                                     qt.QMainWindow, qt]);
    dbg.main(proc);
    return "debugger-entry";
  }
}
