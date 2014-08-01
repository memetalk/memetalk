.preamble(io)
  io: meme:io;
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
instance_method connect: fun(signal, fn) {
  <primitive "qt_qwidget_connect">
}
end

class QPushButton < QWidget
init new: fun(label, parent) {
  <primitive "qt_qpushbutton_new">
}
end

main: fun() {
  var app = QApplication.new();
  // var w  = QWidget.new;
  var b = QPushButton.new("hello", null);
  b.connect("clicked", fun() { io.print("CLICKED!") });
  b.show();
  //w.show();
  app.exec();
}

.endcode
