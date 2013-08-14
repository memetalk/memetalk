module foo()
{
  class X {
    fields: x;
    init new: fun(x) {
      @x = x;
    }
    instance_method x: fun() {
      return @x;
    }
    class_method throw: fun() {
      return 10;
    }
  }

  main: fun() {
    assert(X.throw + X.new(2).x == 12, "Testing class method");
  }
}
