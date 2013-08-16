module foo() {
  main: fun() {
    var cmod = get_compiled_module(thisModule);

    var cfn = CompiledFunction.newTopLevel(
      "bar", "fun(a) { return fun() { a }; }", cmod, :module_function);

    var fn = cfn.instantiate(thisModule);

    assert(fn(9)() == 9, "Compiling top leve function returning closure");
  }
}
