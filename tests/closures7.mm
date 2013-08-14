module foo() {
  main: fun() {
    var cmod = get_compiled_module(thisModule);

    var cfn = CompiledFunction.newTopLevel(
      "bar", "fun(a) { return 10 + a; }", cmod);

    cmod.addFunction(cfn);

    assert(bar(10) == 20,
           "Creating top level fun, adding and accessing from module");
  }
}
