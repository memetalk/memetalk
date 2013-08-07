module foo() {

  fun print(arg) {
    <primitive "print">
  }

  fun main() {
    var cmod = get_compiled_module(thisModule);
    var env = {"a": 1};
    var cfun = CompiledFunction.new("fun(x) { a = a + x; }", [], cmod, env);
    var fn = cfun.asContext(thisModule, null, env);
    var f = fn.apply([]);
    var ret = f(20);
    var env_new = fn.getEnv();
    return env_new;
  }
}
