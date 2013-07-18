module ex(qt)
  qt:  memetalk/qt/1.0();
{
  fun main() {
    var app = qt.QApplication.new();
    var w = qt.QWidget.new(null);
    w.show();
    app.exec();
  }
}
