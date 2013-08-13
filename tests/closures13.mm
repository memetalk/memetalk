module foo()
{
  fun evalFn(text, imodule, frame) {
    var cmod = get_compiled_module(imodule);
    var cfun = CompiledFunction.new("bar",text, [], cmod);
    return cfun.asContext(imodule, frame);
  }

  fun main() {
    var locc = 10;
    var fn = fun() {
      evalFn("locc = 98;", thisModule, get_current_process().stackFrames()[-2]); //-1 == stackFrames()
    }();
    var new_value = fn.apply([]);
    var env = fn.getEnv();
    assert(locc == 98, "nested CompiledFunction.asContext changing value of stack frame");
  }
}
