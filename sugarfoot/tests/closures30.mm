.preamble(test)
.code

main: fun() {
  var fn = Context.withVars("var b = a + 5;", {:a: 5}, thisModule);
  fn();
  test.assert(fn.getEnv().size() == 2 and fn.getEnv()[:a] == 5 and fn.getEnv()[:b] == 10, "testing Context.getEnv");
}

.endcode
