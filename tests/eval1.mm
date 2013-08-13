module foo() {

  main: fun() {
    var cmod = get_compiled_module(thisModule);
    var cfun = CompiledFunction.new("bar","x + 1", ['x'], cmod);
    var fn = cfun.instantiate(thisModule);
    assert(fn.apply([2]) == 3, "CompiledFunction.instantiate");
  }
}
