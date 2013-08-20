.license
Copyright (c) 2012-2013 Thiago B. L. Silva <thiago@metareload.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
.endlicense

.preamble(io)
  io : memetalk/io/1.0();

.code

// -- module functions --

qapp_running: fun() {
  <primitive "qapp_running">
}

// -- module classes --

class QAction
fields: self;
init new: fun(label, parent) {
  <primitive "qt_qaction_new">
}

instance_method connect: fun(signal, slot) {
  <primitive "qt_qaction_connect">
}

instance_method setEnabled: fun(val) {
  <primitive "qt_qaction_set_enabled">
}

instance_method setShortcut: fun(shortcut) {
  <primitive "qt_qaction_set_shortcut">
}

instance_method setShortcutContext: fun(context) {
  <primitive "qt_qaction_set_shortcut_context">
}

end //qt:QAction

class QApplication
fields: self;
init new: fun() {
  <primitive "qt_qapplication_new">
}

instance_method exec: fun() {
  <primitive "qt_qapplication_exec">
}

instance_method exit: fun(code) {
  <primitive "qt_qapplication_exit">
}

class_method focusWidget: fun() {
  <primitive "qt_qapplication_focus_widget">
}

end //qt:QApplication

class QComboBox < QWidget
fields: ;
init new: fun(parent) {
    <primitive "qt_qcombobox_new">
}

instance_method addItem: fun(item) {
  <primitive "qt_qcombobox_add_item">
}

instance_method clear: fun() {
  <primitive "qt_qcombobox_clear">
}

instance_method setCurrentIndex: fun(i) {
  <primitive "qt_qcombobox_set_current_index">
}

end //qt:QComboBox

class QEventLoop
fields: self;
init new: fun() {
  <primitive "qt_qeventloop_new">
}

instance_method exec: fun() {
  <primitive "qt_qeventloop_exec">
}

instance_method exit: fun(code) {
  <primitive "qt_qeventloop_exit">
}

end //qt:QEventLoop

class QHBoxLayout < QLayout
fields: ;
init new: fun(parent) {
  <primitive "qt_qhboxlayout_new">
}

instance_method addLayout: fun(layout) {
  <primitive "qt_qhboxlayout_add_layout">
}

instance_method addWidget: fun(widget) {
  <primitive "qt_qhboxlayout_add_widget">
}

instance_method setContentsMargins: fun(l,t,r,b) {
  <primitive "qt_qhboxlayout_set_contents_margins">
}

end //qt:QHBoxLayout

class QHeaderView
fields: self;
instance_method hide: fun() {
  <primitive "qt_qheaderview_hide">
}

instance_method setStretchLastSection: fun(val) {
  <primitive "qt_qheaderview_set_stretch_last_section">
}

end //qt:QHeaderView

class QLabel < QWidget
fields: ;
init new: fun(parent) {
    <primitive "qt_qlabel_new">
}

instance_method setText: fun(text) {
    <primitive "qt_qlabel_set_text">
}

end //qt:QLabel

class QLayout
fields: self;
instance_method addWidget: fun(widget) {
  <primitive "qt_qlayout_add_widget">
}

end //qt:QLayout

class QLineEdit < QWidget
fields: ;
init new: fun(parent) {
  <primitive "qt_qlineedit_new">
}

instance_method selectAll: fun() {
  <primitive "qt_qlineedit_select_all">
}

instance_method selectedText: fun() {
  <primitive "qt_qlineedit_selected_text">
}

instance_method setSelection: fun(start,length) {
  <primitive "qt_qlineedit_set_selection">
}

instance_method setText: fun(text) {
  <primitive "qt_qlineedit_set_text">
}

instance_method text: fun() {
  <primitive "qt_qlineedit_text">
}

end //qt:QLineEdit

class QListWidget < QWidget
fields: ;
init new: fun(parent) {
  <primitive "qt_qlistwidget_new">
}

instance_method currentItem: fun() {
  <primitive "qt_qlistwidget_current_item">
}

end //qt:QListWidget

class QListWidgetItem
fields: self;
init new: fun(text, parent) {
  <primitive "qt_qlistwidgetitem_new">
}

instance_method text: fun() {
  <primitive "qt_qlistwidgetitem_text">
}

end //qt:QListWidgetItem

class QMainWindow < QWidget
fields: ;
init new: fun() {
  <primitive "qt_qmainwindow_new">
}

instance_method menuBar: fun() {
  <primitive "qt_qmainwindow_menu_bar">
}

instance_method setCentralWidget: fun(widget) {
  <primitive "qt_qmainwindow_set_central_widget">
}

instance_method statusBar: fun() {
  <primitive "qt_qmainwindow_status_bar">
}

end //qt:QMainWindow

class QMenu < QWidget
fields: ;
end //qt:QMenu

class QMenuBar < QWidget
fields: ;
instance_method addMenu: fun(str) {
  <primitive "qt_qmenubar_add_menu">
}

end //qt:QMenuBar

class QPlainTextEdit < QWidget
fields: ;
init new: fun(parent) {
  <primitive "qt_qplaintextedit_new">
}

instance_method setPlainText: fun(text) {
  <primitive "qt_qplaintextedit_set_plain_text">
}

instance_method setTabStopWidth: fun(val) {
  <primitive "qt_qplaintextedit_set_tabstop_width">
}

instance_method setTextCursor: fun(cursor) {
  <primitive "qt_qplaintextedit_set_text_cursor">
}

instance_method textCursor: fun() {
  <primitive "qt_qplaintextedit_text_cursor">
}

instance_method toPlainText: fun() {
  <primitive "qt_qplaintextedit_to_plain_text">
}

end //qt:QPlainTextEdit

class QsciScintilla < QWidget
fields: ;
init new: fun(parent) {
    <primitive "qt_scintilla_editor_new">
}

instance_method getCursorPosition: fun() {
    <primitive "qt_scintilla_get_cursor_position">
}

instance_method getSelection: fun() {
    <primitive "qt_scintilla_get_selection">
}

instance_method insertAt: fun(text, line, index) {
    <primitive "qt_scintilla_insert_at">
}

instance_method pausedAtLine: fun(start_line, start_col, end_line, end_col) {
    <primitive "qt_scintilla_paused_at_line">
}

instance_method selectedText: fun() {
    <primitive "qt_scintilla_selected_text">
}

instance_method setSelection: fun(start_line, start_index, end_line, end_index) {
    <primitive "qt_scintilla_set_selection">
}

instance_method setText: fun(text) {
    <primitive "qt_scintilla_set_text">
}

instance_method text: fun() {
    <primitive "qt_scintilla_text">
}

end //qt:QsciScintilla

class QShortcut
fields: self;
init new: fun(sc, parent, slot) {
  <primitive "qt_qshortcut_new">
}

instance_method setContext: fun(context) {
  <primitive "qt_qshortcut_set_context">
}

end //qt:QShortcut

class QTableWidget < QWidget
fields: ;
init new: fun(rows, cols, parent) {
    <primitive "qt_qtablewidget_new">
}

instance_method clear: fun() {
  <primitive "qt_qtablewidget_clear">
}

instance_method horizontalHeader: fun() {
  <primitive "qt_qtablewidget_horizontal_header">
}

instance_method setHorizontalHeaderLabels: fun(labels) {
  <primitive "qt_qtablewidget_set_horizontal_header_labels">
}

instance_method setItem: fun(line,col,item) {
  <primitive "qt_qtablewidget_set_item">
}

instance_method setSelectionMode: fun(mode) {
  <primitive "qt_qtablewidget_set_selection_mode">
}

instance_method setSortingEnabled: fun(val) {
  <primitive "qt_qtablewidget_set_sorting_enabled">
}

instance_method verticalHeader: fun() {
  <primitive "qt_qtablewidget_vertical_header">
}

end //qt:QTableWidget

class QTableWidgetItem
fields: self;
init new: fun(label) {
  <primitive "qt_qtablewidgetitem_new">
}

instance_method setFlags: fun(flags) {
  <primitive "qt_qtablewidgetitem_set_flags">
}

end //qt:QTableWidgetItem

class QTextCursor
fields: ;
instance_method dragRight: fun(len) {
  <primitive "qt_qtextcursor_drag_right">
}

instance_method insertText: fun(text) {
  <primitive "qt_qtextcursor_insert_text">
}

instance_method selectedText: fun() {
  <primitive "qt_qtextcursor_selected_text">
}

instance_method selectionEnd: fun() {
  <primitive "qt_qtextcursor_selection_end">
}

instance_method setPosition: fun(pos) {
  <primitive "qt_qtextcursor_set_position">
}

end //qt:QTextCursor

class QUrl
fields: self;
instance_method fragment: fun() {
  <primitive "qt_qurl_fragment">
}

instance_method hasFragment: fun() {
  <primitive "qt_qurl_has_fragment">
}

instance_method path: fun() {
  <primitive "qt_qurl_path">
}

instance_method queryItemValue: fun(name) {
  <primitive "qt_qurl_query_item_value">
}

instance_method hasQueryItem: fun(name) {
  <primitive "qt_qurl_has_query_item">
}

instance_method toString: fun() {
  <primitive "qt_qurl_to_string">
}

end //qt:QUrl

class QVBoxLayout < QLayout
fields: ;
init new: fun(parent) {
  <primitive "qt_qvboxlayout_new">
}

instance_method addLayout: fun(layout) {
  <primitive "qt_qvboxlayout_add_layout">
}

instance_method addWidget: fun(widget) {
  <primitive "qt_qvboxlayout_add_widget">
}

end //qt:QVBoxLayout

class QWebElement
fields: self;
instance_method appendInside: fun(val) {
  <primitive "qt_qwebelement_append_inside">
}

instance_method appendOutside: fun(val) {
  <primitive "qt_qwebelement_append_outside">
}

instance_method clone: fun() {
  <primitive "qt_qwebelement_clone">
}

instance_method findFirst: fun(str) {
  <primitive "qt_qwebelement_find_first">
}

instance_method setAttribute: fun(name, val) {
  <primitive "qt_qwebelement_set_attribute">
}

instance_method setInnerXml: fun(xml) {
  <primitive "qt_qwebelement_set_inner_xml">
}

instance_method setPlainText: fun(str) {
  <primitive "qt_qwebelement_set_plain_text">
}

instance_method setStyleProperty: fun(name, val) {
  <primitive "qt_qwebelement_set_style_property">
}

instance_method takeFromDocument: fun() {
  <primitive "qt_qwebelement_take_from_document">
}

instance_method toOuterXml: fun() {
  <primitive "qt_qwebelement_to_outer_xml">
}

end //qt:QWebElement

class QWebFrame
fields: self;
instance_method addToJavaScriptWindowObject: fun(name, obj) {
  <primitive "qt_qwebframe_add_to_javascript_window_object">
}

instance_method documentElement: fun() {
  <primitive "qt_qwebframe_document_element">
}

instance_method scrollToAnchor: fun(anchor) {
  <primitive "qt_qwebframe_scroll_to_anchor">
}

end //qt:QWebFrame

class QWebPage
fields: self;
instance_method enablePluginsWith: fun(name,fn) {
  <primitive "qt_extra_qwebpage_enable_plugins">
}

instance_method mainFrame: fun() {
  <primitive "qt_qwebpage_main_frame">
}

instance_method setLinkDelegationPolicy: fun(policy) {
  <primitive "qt_qwebpage_set_link_delegation_policy">
}

end //qt:QWebPage

class QWebView < QWidget
fields: ;
init new: fun(parent) {
  <primitive "qt_qwebview_new">
}

instance_method loadUrl: fun(url) {
  this.setHtml(io.file_contents(url));
}

instance_method page: fun() {
  <primitive "qt_qwebview_page">
}

instance_method setHtml: fun(html) {
  <primitive "qt_qwebview_set_html">
}

instance_method setUrl: fun(url) {
  <primitive "qt_qwebview_set_url">
}

end //qt:QWebView

class QWidget
fields: self;
init new: fun(parent) {
  <primitive "qt_qwidget_new">
}

instance_method actions: fun() {
  <primitive "qt_qwidget_actions">
}

instance_method addAction: fun(action) {
  <primitive "qt_qwidget_add_action">
}

instance_method close: fun() {
  <primitive "qt_qwidget_close">
}

instance_method connect: fun(signal, slot) {
  <primitive "qt_qwidget_connect">
}

instance_method hasFocus: fun() {
  <primitive "qt_qwidget_has_focus">
}

instance_method hide: fun() {
  <primitive "qt_qwidget_hide">
}

instance_method isVisible: fun() {
  <primitive "qt_qwidget_is_visible">
}

instance_method resize: fun(w,h) {
  <primitive "qt_qwidget_resize">
}

instance_method setFocus: fun() {
  <primitive "qt_qwidget_set_focus">
}

instance_method setMaximumHeight: fun(h) {
  <primitive "qt_qwidget_set_maximum_height">
}

instance_method setMaximumWidth: fun(w) {
  <primitive "qt_qwidget_set_maximum_width">
}

instance_method setMinimumSize: fun(w,h) {
  <primitive "qt_qwidget_set_minimum_size">
}

instance_method setMinimumWidth: fun(w) {
  <primitive "qt_qwidget_set_minimum_width">
}

instance_method setStyleSheet: fun(s) {
  <primitive "qt_qwidget_set_stylesheet">
}

instance_method setWindowTitle: fun(title) {
  <primitive "qt_qwidget_set_window_title">
}

instance_method show: fun() {
  <primitive "qt_qwidget_show">
}

end //qt:QWidget


.end
