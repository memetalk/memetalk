module foo() {

  fun main() {
    var cmod = get_compiled_module(thisModule);
    var env = {"c": 1};
    var cfun = CompiledFunction.new("bar","var a = 10 + b + c; a;", ['b'], cmod);
    var fn = cfun.asContext(thisModule, env);
    var res = fn.apply([20]);
    assert(res == 31, "CompiledFunction.asContext declaring local var");
  }
}
