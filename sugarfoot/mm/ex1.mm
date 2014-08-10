.preamble(qt, io)
  qt : meme:qt;
  io : meme:io;
  [QWidget] <= qt;
.code

class QPushButton < QWidget
init new: fun(label, parent) {
  <primitive "qt_qpushbutton_new">
}
end

main: fun() {
  var app = qt.QApplication.new();
  var w = qt.QWidget.new(null);
  var button = QPushButton.new("oi", w);
  button.connect("pressed", fun() { io.print("pressed") });
  w.show();
  app.exec();
}

.endcode
