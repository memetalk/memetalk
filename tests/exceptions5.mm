module foo()
{
  fun r(fn) {
    fn();
    Exception.throw("xx");
    fn();
  }

  fun b(fn) {
    fn();
    try {
      fn();
      r(fn);
      fn();
    } catch(e) {
      fn();
    }
    fn();
    Exception.throw("yy");
    fn();
  }

  fun a(fn) {
    fn();
    try {
      fn();
      b(fn);
      fn();
    } catch(e) {
      fn();
    }
    fn();
  }

  fun main() {
    var x = 0;
    var fn = fun() { x = x + 1; };
    a(fn);
    assert(x == 9, "Multiple try/catches");
  }
}
