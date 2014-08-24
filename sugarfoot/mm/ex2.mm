.preamble()
.code

class X
class_method z: fun() {
  return 100;
}
end


t1: fun() {
  var fn = Context.withVars("\"10\"", null, thisModule);
  return fn();
}

t2: fun() {
  var fn = Context.withVars("X.z", null, thisModule);
  return fn();
}

t3: fun() {
  var fn = Context.withVars("this", {:this: 3}, thisModule);
  return fn();
}

t4: fun() {
  var fn = Context.withVars("a", {:a: 4}, thisModule);
  return fn();
}

t5: fun() {
  var fn = Context.withVars("a + X.z", {:a: 5}, thisModule);
  return fn();
}

main: fun() {
  // return t1();
  // return t2();
  // return t3();
  // return t4();
  return t5();

  // var a = 10;
  // var b = 20;
  // var x = fun(c) { var d = 9; d };
  // return thisContext.getEnv();
}

.endcode
