.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var v = {"c": 2, "d":100};
  var z = 1;

  var cfn = CompiledFunction.newClosure(
    "fun(a) { a + 10 }", thisContext.compiledFunction(), false);

  cfn.setCode("fun(a,b) { return a + b + c; }");

  var fn = cfn.asContextWithVars(thisModule, v);

  assert(fn(2,3) + fn.getEnv()["d"] + z == 108 ,
         "Changing closure code with var dict");
}

// -- module classes --


.end
