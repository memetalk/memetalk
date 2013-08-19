.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var v = {"b": 99, "c": 2};
  var cfn = CompiledFunction.newClosure(
    "fun(a) { fun() { c = 3; a + b; } }", thisContext.compiledFunction(),false);
  var fn = cfn.asContextWithVars(thisModule, v);
  assert(fn(1)() + fn.getEnv()["c"] == 103,
         "Closure with var dict returning closure");
}

// -- module classes --


.end
