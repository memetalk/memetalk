.preamble()
.code

class QApplication
fields: self;
init new: fun() {
  <primitive "qt_qapplication_new">
}
instance_method exec: fun() {
  <primitive "qt_qapplication_exec">
}
end

class QWidget
fields: self;
init new: fun() {
  <primitive "qt_qwidget_new">
}
instance_method show: fun() {
  <primitive "qt_qwidget_show">
}
end

main: fun() {
  var app = QApplication.new();
  var w = QWidget.new();
  w.show();
  app.exec();
}

.endcode
