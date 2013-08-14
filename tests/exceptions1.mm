module foo()
{
  class MyException {
    fields: x;
    init new: fun(x) {
      @x = x;
    }
    instance_method raise: fun() {
       <primitive "exception_raise">
    }
    instance_method x: fun() {
      return @x;
    }
    class_method throw: fun(x) {
      var self = MyException.new(x);
      self.raise();
    }
  }

  r: fun(fn) {
    fn(1);
    MyException.throw(10);
    assert(false, "r(): shouldn't execute here");
  }

  i: fun(fn) {
    fn(1);
    r(fn);
    assert(false, "i(): shouldn't execute here");
  }

  main: fun() {
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
