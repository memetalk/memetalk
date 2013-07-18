module ex(qt)
  qt:  memetalk/qt/1.0();
  [QApplication, QWidget] <= qt;
{
  class Foo < QWidget {
    init new() {
      super.new(null);
      this.setWindowTitle("Hello");
    }
  }

  fun main() {
    var app = QApplication.new();
    var w = Foo.new();
    w.show();
    app.exec();
  }
}
