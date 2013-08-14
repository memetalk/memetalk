module foo() {
  main: fun() {
    var cmod = get_compiled_module(thisModule);

    var cfn = CompiledFunction.newTopLevel(
      "bar", "fun(a) { return 10 + a; }", cmod);

    var fn = cfn.instantiate(thisModule);

    assert(fn(1) == 11, "Creating top level function");
  }
}
