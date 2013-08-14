module foo() {
  main: fun() {
    var k = 9;
    return fun(x) {
      var y = 10;
      var cfn = CompiledFunction.newClosure("fun(a) { fun() { x = x + 20; a + y + k; } }", thisContext.compiledFunction());
      var fn = cfn.asContextWithFrame(thisModule, get_current_process().stackFrames()[-2]);
      assert(fn(1)() + x == 140, "Closure with frame inside closure accessing outer closure and top level vars");
    }(100);
  }
}
