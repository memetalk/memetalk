module foo() {

  fun getClass_CompiledFunction() {
    <primitive "class_compiled_function">
  }

  fun get_current_compiled_module() {
    <primitive "get_current_compiled_module">
  }

  fun main() {
    var CompiledFunction = getClass_CompiledFunction();
    var cmod = get_current_compiled_module();
    var env = {"a": 10,'b':0};
    var cfun = CompiledFunction.new("return a + b;", ['b'], cmod, env);
    var fn = cfun.asContext(thisModule, env);
    var res = fn.apply([20]);
    return res; // 30
  }
}
