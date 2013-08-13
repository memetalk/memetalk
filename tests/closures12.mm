module foo()
{
  evalFn: fun(text, imodule, frame) {
    var cmod = get_compiled_module(imodule);
    var cfun = CompiledFunction.new("bar",text, [], cmod);
    return cfun.asContext(imodule, frame);
  }

  main: fun() {
    var locc = 10;
    var fn = evalFn("locc = 99;", thisModule, get_current_process().stackFrames()[-2]); //-1 == stackFrames()
    var new_value = fn.apply([]);
    assert(locc == 99, "CompiledFunction.asContext changing value of stack frame");
  }
}
