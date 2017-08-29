.license
.endlicense

.preamble(test)

.code

baz: fun() {
  Exception.throw("baz_ex");
  test.assert(false, "should not execute this");
}

bar: fun() {
  baz();
  test.assert(false, "should not execute this");
}

foo: fun() {
  <primitive "test_catch_exception">
}

main: fun() {
  test.assertEqual(foo().message, "baz_ex", "Primitive handled exception");
}

.endcode
