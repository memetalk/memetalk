module ex()
{
  fun evalFn(text, imodule, frame) {
    var cmod = get_compiled_module(imodule);
    var cfun = CompiledFunction.new(text, [], cmod);
    return cfun.asContext(imodule, frame);
  }

  fun main() {
    var locc = 10;
    var fn = evalFn("locc = 99;", thisModule, get_current_process().stackFrames()[-2]); //-1 == stackFrames()
    var new_value = fn.apply([]);
    return locc;
  }
}
