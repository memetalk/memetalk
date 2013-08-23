.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var k = 9;
  return fun(x) {
    var y = 10;

    var cfn = CompiledFunction.newClosure(
      "fun(a) { fun() { x = x + 20; a + y + k; } }",
      thisContext.compiledFunction(), false);

    var fn = cfn.asContextWithFrame(
      thisModule, VMProcess.current.stackFrames()[-2]);

    assert(fn(1)() + x == 140,
           "Closure with frame inside closure accessing outer closure and top level vars");
  }(100);
}

// -- module classes --


.end
