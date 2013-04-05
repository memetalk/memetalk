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
    var env = {"a": 9};
    var cfun = CompiledFunction.new("fun() { a }", [], cmod, env);
    var fn = cfun.asContext(thisModule, env);
    var res = fn.apply([]);
    return res();
  }
}
