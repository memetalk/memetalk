module ex()
{
  class X {
    init new() {
      return this;
    }
    fun y(fn) {
      fn(1);
    }
    fun x(fn) {
      fn(2);
    }
  }
  fun main() {
    var res = 0;
    var e = X.new();
    e.x(fun(m) {
      if (true) {
        e.y(fun(n) {
          res = m + n;
        });
      }
    });
    return res;
  }
}
