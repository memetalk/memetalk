module foo() {

  fun print(arg) {
    <primitive "print">
  }

  fun getClass_CompiledFunction() {
    <primitive "class_compiled_function">
  }

  fun get_current_compiled_module() {
    <primitive "get_current_compiled_module">
  }

  fun main() {
    var CompiledFunction = getClass_CompiledFunction();
    var cmod = get_current_compiled_module();
    var env = {"a": 1};
    var cfun = CompiledFunction.new("fun(x) { a = a + x; }", [], cmod, env);
    var fn = cfun.asContext(thisModule, env);
    var f = fn.apply([]);
    var ret = f(20);
    var env_new = fn.getEnv();
    return env_new;
  }
}
