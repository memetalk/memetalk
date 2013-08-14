module foo() {
  class X {
    class_method x: fun() {
      return thisContext;
    }
  }

  main: fun() {
    var fn = X.x;
    fn.compiledFunction.setCode("fun(a) { return a + 10; }");
    assert(X.x(10) == 20, "Changing class method");
  }
}
