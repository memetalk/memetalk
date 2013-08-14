module foo() {
  class X {
    fields: x;
    init new: fun(x) {
      @x = x;
    }
    instance_method x: fun() {
      return @x;
    }
  }

  main: fun() {
    var x1 = X.new(1);
    var xc = get_compiled_class(X);
    var ctor = xc.constructors()['new'];
    ctor.setCode("fun(x) { @x = x + 100; }");
    var x2 = X.new(2);
    assert(x1.x + x2.x == 103, "Changing constructor");
  }
}
