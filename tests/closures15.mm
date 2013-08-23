.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var x = 100;
  var y = 10;
  var cfn = CompiledFunction.newClosure(
    "fun(a) { x = 20; a + y; }", thisContext.compiledFunction(), false);
  var fn = cfn.asContextWithFrame(
    thisModule, VMProcess.current.stackFrames()[-2]);
  assert(fn(1) + x == 31, "Creating closure with frame");
}

// -- module classes --


.end
