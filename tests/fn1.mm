module ex(qt)
  qt:  memetalk/qt/1.0();
{
  fn QWidget = qt.QWidget;
  fn QApplication = qt.QApplication;

  fun main() {
    var app = QApplication().new();
    var w = QWidget().new(null);
    w.show();
    app.exec();
  }
}
