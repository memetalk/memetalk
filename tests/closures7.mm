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
    var env = {"c": 1};
    var cfun = CompiledFunction.new("var a = 10 + b + c; a;", ['b'], cmod, env);
    var fn = cfun.asContext(thisModule, null, env);
    var res = fn.apply([20]);
    return res;
  }
}
