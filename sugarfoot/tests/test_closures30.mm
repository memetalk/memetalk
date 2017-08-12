.preamble(test)
.code

main: fun() {
  var fn = Context.withVars("var b = a + 5;", {:a: 5}, thisModule);
  fn();
  test.assertEqual(fn.getEnv().size(), 2, "testing Context.getEnv size");
  test.assertEqual(fn.getEnv()[:a], 5, "testing Context.getEnv [a]");
  test.assertEqual(fn.getEnv()[:b], 10, "testing Context.getEnv [b]");
}

.endcode
