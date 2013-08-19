.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  return fun(x) {
    var y = 10;
    var v = {"b": 2, "c":2};

    var cfn = CompiledFunction.newClosure(
      "fun(a) { c = 3; var k = a + b; k; }",
      thisContext.compiledFunction(), false);

    //can't access x because we are using specific vars dict
    var fn = cfn.asContextWithVars(thisModule, v);

    assert(fn(1) + fn.getEnv()["c"] == 6,
           "Creating closure /w vardict inside closure");
  }(20);
}

// -- module classes --


.end
