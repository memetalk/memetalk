.license
.endlicense

.preamble(io)
  io : memetalk/io/1.0();

.code

// -- module functions --

foo: fun() {
  var cfn = CompiledFunction.newClosure(
    "fun(a) { a }", thisContext.compiledFunction(), false);

  var fn = cfn.asContextWithVars(thisModule, {});
  assert(fn(99) == 99, "Testing closures screwing with toplevel");
}

main: fun() {
  foo(); //screwing with foo's env
  foo(); //check if it's all ok
}

// -- module classes --


.end
