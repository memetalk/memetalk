module foo() {

  class X {
    init new: fun() {
    }
    instance_method bar: fun() {
      return 42;
    }
  }

  main: fun() {
    var x = X.new;
    var klass = get_compiled_class(X);
    klass.removeInstanceMethod("bar");
    try {
      x.bar();
      assert(false, "Shouldn't be here");
    } catch(e) {
      assert(e, "x.bar does not exist anymore");
    }
    return 1;
  }
}
