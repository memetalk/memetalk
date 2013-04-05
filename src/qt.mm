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
    fun setMaximumWidth(w) {
      <primitive "qt_qwidget_set_maximum_width">
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
    fun setTextCursor(cursor) {
      <primitive "qt_qplaintextedit_set_text_cursor">
    }
  }

  class QTextCursor {
    fun selectedText() {
      <primitive "qt_qtextcursor_selected_text">
    }
    fun selectionEnd() {
      <primitive "qt_qtextcursor_selection_end">
    }
    fun setPosition(pos) {
      <primitive "qt_qtextcursor_set_position">
    }
    fun insertText(text) {
      <primitive "qt_qtextcursor_insert_text">
    }
    fun dragRight(len) {
      <primitive "qt_qtextcursor_drag_right">
    }
  }

  class QLayout {
    fields: self;
    fun addWidget(widget) {
      <primitive "qt_qlayout_add_widget">
    }
  }
  class QVBoxLayout < QLayout {
    init new(parent) {
      <primitive "qt_qvboxlayout_new">
    }
    fun addLayout(layout) {
      <primitive "qt_qvboxlayout_add_layout">
    }
  }

  class QHBoxLayout < QLayout {
    init new(parent) {
      <primitive "qt_qhboxlayout_new">
    }
    fun addLayout(layout) {
      <primitive "qt_qhboxlayout_add_layout">
    }
  }


  class QListWidget < QWidget {
    init new(parent) {
      <primitive "qt_qlistwidget_new">
    }
    fun connect(signal, slot) {
      <primitive "qt_qlistwidget_connect">
    }
    fun addItem(string) {
      <primitive "qt_qlistwidget_add_item">
    }
    fun clear() {
      <primitive "qt_qlistwidget_clear">
    }
    fun clearSelection() {
      <primitive "qt_qlistwidget_clear_selection">
    }
    fun selectedItems() {
      <primitive "qt_qlistwidget_selected_items">
    }
    fun removeItemWidget(item) {
      <primitive "qt_qlistwidget_remove_item_widget">
    }
  }

  /* QListWidgetItem { */
  /*   fields: self; */
  /*   init new(text, parent) { */
  /*     <primitive "qt_qlistwidgetitem_new"> */
  /*   } */
  /*   fun text() { */
  /*     <primitive "qt_qlistwidgetitem_text"> */
  /*   } */
  /* } */

  class QLineEdit < QWidget {
    init new(parent) {
      <primitive "qt_qlineedit_new">
    }
    fun setFocus() {
      <primitive "qt_qlineedit_set_focus">
    }
  }
}
