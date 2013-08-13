module foo() {

  fun main() {
    var cmod = get_compiled_module(thisModule);
    var env = {"a": 9};
    var cfun = CompiledFunction.new("bar","fun() { a }", [], cmod);
    var fn = cfun.asContext(thisModule, env);
    var res = fn.apply([]);
    assert(res() == 9, "CompiledFunction.asContext returning closure value");
  }
}
