module foo() {

  fun main() {
    var cmod = get_compiled_module(thisModule);
    var cfun = CompiledFunction.new("x + 1", ['x'], cmod, {});
    var fn = cfun.instantiate(thisModule);
    return fn.apply([2]);
  }
}
