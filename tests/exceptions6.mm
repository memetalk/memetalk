.license
.endlicense

.preamble()

.code

// -- module functions --

doit: fun(fn) {
  return fn("number()");
}

evalWithVars: fun(text, vars) {
  var fn = evalWithVarsFn(text, vars);
  var res = fn();
  return {"result": res, "env": fn.getEnv()};
}

evalWithVarsFn: fun(text, vars) {
  var cmod = get_compiled_module(thisModule);
  var code = "fun() {" + text + "}";
  var cfn = CompiledFunction.newClosure(code, thisContext.compiledFunction(), false);
  return cfn.asContextWithVars(thisModule, vars);
}

main: fun() {
  return doit(fun(expr) {
      try {
        var r = evalWithVars(expr, {});
        assert(r["result"] == 42, "Testing nested try/catch with closures");
      } catch(e) {
        assert(false, "Shouldn't be here");
      }
  });
}

number: fun() {
  return 42;
}

// -- module classes --


.end
