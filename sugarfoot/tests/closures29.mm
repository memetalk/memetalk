.preamble(test)
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

t6: fun() {
  var fn = Context.withVars(" {{ ", {}, thisModule);
  return fn();
}

main: fun() {
  test.assert(t1() == "10", "closure returning literal");
  test.assert(t2() == 100, "closure acessing module entry");
  test.assert(t3() == 3, "closure accessing this");
  test.assert(t4() == 4, "closure accessing specified value");
  test.assert(t5() == 105, "more complex expression in closure");

  try {
    t6();
    test.assert(false, "Compilation should have failed");
  } catch(CompileError e) {
    test.assert(true, "Compilation succeeded");
  }
}

.endcode
