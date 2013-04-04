module qt() {

  class QApplication {
    fields: self;
    init new() {
      <primitive "qt_qapplication_new">
    }
    fun exec() {
      <primitive "qt_qapplication_exec">
    }
  }

  class QWidget {
    fields: self;
    init new(parent) {
      <primitive "qt_qwidget_new">
    }
    fun setWindowTitle(title) {
      <primitive "qt_qwidget_set_window_title">
    }
    fun resize(w,h) {
      <primitive "qt_qwidget_resize">
    }
    fun show() {
      <primitive "qt_qwidget_show">
    }
    fun addAction(action) {
      <primitive "qt_qwidget_add_action">
    }
  }

  class QMenuBar < QWidget {
    fun addMenu(str) {
      <primitive "qt_qmenubar_add_menu">
    }
  }

  class QMenu < QWidget {
  }

  class QAction {
    fields: self;
    init new(label, parent) {
      <primitive "qt_qaction_new">
    }
    fun connect(signal, slot) {
      <primitive "qt_qaction_connect">
    }
    fun setShortcut(shortcut) {
      <primitive "qt_qaction_set_shortcut">
    }
  }

  class QMainWindow < QWidget {
    init new() {
      <primitive "qt_qmainwindow_new">
    }
    fun setCentralWidget(widget) {
      <primitive "qt_qmainwindow_set_central_widget">
    }
    fun menuBar() {
      <primitive "qt_qmainwindow_menu_bar">
    }
  }

  class QPlainTextEdit < QWidget {
    init new(parent) {
      <primitive "qt_qplaintextedit_new">
    }
    fun setTabStopWidth(val) {
      <primitive "qt_qplaintextedit_set_tabstop_width">
    }
    fun textCursor() {
      <primitive "qt_qplaintextedit_text_cursor">
    }
  }

  class QTextCursor {
    fun selectedText() {
      <primitive "qt_qtextcursor_selected_text">
    }
    /* fun selectionEnd() { */
    /*   <primitive "qt_qtextcursor_selection_end"> */
    /* } */
    /* fun setPosition(pos) { */
    /*   <primitive "qt_qtextcursor_set_position"> */
    /* } */
    /* fun insertText(text) { */
    /*   <primitive "qt_qtextcursor_insert_text"> */
    /* } */
    /* fun dragRight(len) { */
    /*   <primitive "qt_qtextcursor_drag_right"> */
    /* } */
  }
}
