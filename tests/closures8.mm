module foo() {

  fun main() {
    var cmod = get_compiled_module(thisModule);
    var env = {"a": 9};
    var cfun = CompiledFunction.new("fun() { a }", [], cmod, env);
    var fn = cfun.asContext(thisModule, null, env);
    var res = fn.apply([]);
    return res();
  }
}
