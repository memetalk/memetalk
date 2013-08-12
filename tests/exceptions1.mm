module foo()
{
  class MyException {
    fields: x;
    init new(x) {
      @x = x;
    }
    func throw(x) {
      var self = MyException.new(x);
      self.raise();
    }
    fun raise() {
       <primitive "exception_raise">
    }
    fun x() {
      return @x;
    }
  }

  fun r(fn) {
    fn(1);
    MyException.throw(10);
    assert(false, "r(): shouldn't execute here");
  }

  fun i(fn) {
    fn(1);
    r(fn);
    assert(false, "i(): shouldn't execute here");
  }

  fun main() {
    var a = 0;
    var fn = fun(x) { a = a + x; };
    try {
      i(fn);
      assert(false, "main: shouldn't execute here");
    } catch(e) {
      assert(e.x() + a == 12, "try/catch and exception flow with custom exception class");
    }
  }
}
