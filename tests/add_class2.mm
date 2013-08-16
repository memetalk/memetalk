module foo() {

  main: fun() {
    var cmod = get_compiled_module(thisModule);
    var klass = cmod.newClass("X");

    var code = "fun() { return 42; }";
    var cfun = CompiledFunction.newTopLevel("bar", code, klass, :class_method);
    klass.addMethod(cfun, :class_method);
    cmod.addClass(klass);
    assert(X.bar == 42, "Testing reading compiled class to module");
  }
}
