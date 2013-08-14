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

  i: fun(fn) {
    fn(1);
  }

   main: fun() {
    var a = 0;
    var fn = fun(x) { a = a + x; };
    try {
      i(fn);
    } catch(e) {
      assert(false, "Shouldn't execute catch");
    }
    assert(a == 1, "Executing code after try/catch");
  }
}
