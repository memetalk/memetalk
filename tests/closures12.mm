module foo() {
  main: fun() {
    var v = {"b": 99, "c": 2};
    var cfn = CompiledFunction.newClosure(
      "fun(a) { c = 3; a + b; }", thisContext.compiledFunction(),false);
    var fn = cfn.asContextWithVars(thisModule, v);
    assert(fn(1) + fn.getEnv()["c"] == 103,
           "Creating closure and accessing var dict");
  }
}
