module foo() {

  fun main() {
    var cmod = get_compiled_module(thisModule);
    var env = {"a": 10,'b':0};
    var cfun = CompiledFunction.new("return a + b;", ['b'], cmod, thisContext.compiledFunction());
    var fn = cfun.asContext(thisModule, env);
    var res = fn.apply([20]);
    return res; // 30
  }
}
