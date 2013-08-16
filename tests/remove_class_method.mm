module foo() {

  class X {
    init new: fun() {
    }
    class_method bar: fun() {
      return 42;
    }
  }

  main: fun() {
    var klass = get_compiled_class(X);
    klass.removeMethod("bar", :class_method);
    try {
      X.bar();
      assert(false, "Shouldn't be here");
    } catch(e) {
      assert(e, "x.bar does not exist anymore");
    }
    return 1;
  }
}
