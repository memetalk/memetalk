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

  fun i(fn) {
    fn(1);
  }

  fun main() {
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
