module foo() {

  class X {
    init new: fun() {
    }
  }

  main: fun() {
    var x = X.new;
    var cmod = get_compiled_module(thisModule);
    var code = "fun() { return 42; }";
    var klass = get_compiled_class(X);
    var cfun = CompiledFunction.newTopLevel("bar", code, klass, :instance_method);
    klass.addInstanceMethod(cfun);
    assert(x.bar == 42, "Adding instance method");
  }
}
