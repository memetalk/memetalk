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
    var cfun = CompiledFunction.new("x + 1", ['x'], cmod);
    var fn = cfun.instantiate(thisModule);
    return fn.apply([2]);
  }
}
