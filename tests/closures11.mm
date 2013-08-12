module foo()
{
  class X {
    init new() {
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
    assert(res == 3, "if inside nested closures triggered by methods");
  }
}
