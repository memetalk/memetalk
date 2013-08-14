module foo() {
  main: fun() {
    var x = 100;
    var y = 10;
    var cfn = CompiledFunction.newClosure("fun(a) { x = 20; a + y; }", thisContext.compiledFunction());
    var fn = cfn.asContextWithFrame(thisModule, get_current_process().stackFrames()[-2]);
    assert(fn(1) + x == 31, "Creating closure with frame");
  }
}
