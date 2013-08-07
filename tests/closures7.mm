module foo() {

  fun main() {
    var cmod = get_compiled_module(thisModule);
    var env = {"c": 1};
    var cfun = CompiledFunction.new("var a = 10 + b + c; a;", ['b'], cmod, env);
    var fn = cfun.asContext(thisModule, null, env);
    var res = fn.apply([20]);
    return res;
  }
}
