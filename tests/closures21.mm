.license
.endlicense

.preamble()

.code

// -- module functions --

foo: fun() {
  var k = 9;
  return fun(x) {
    var y = 10;
    var cmod = get_compiled_module(thisModule);

    var cfn = CompiledFunction.newClosure(
      "fun(a) { fun() { x = x + 20; a + y + k; } }",
      thisContext.compiledFunction(), false);

    var fn = cfn.asContextWithFrame(
      thisModule, get_current_process().stackFrames()[-2]);

    assert(fn(1)() + x == 140,
           "Closure with frame inside closure accessing outer closure and top level vars");

  }(100);
}

main: fun() {
  foo(); //screwing with foo's env
  foo(); //check if it's all ok
}

// -- module classes --


.end
