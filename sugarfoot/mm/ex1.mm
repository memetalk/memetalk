.preamble()
.code

do_print: fun(arg) {
  <primitive "print">
}

print: fun() {
  return fun(arg) { do_print(arg) };
}

test_import: fun(filepath, args) {
  <primitive "test_import">
}

main: fun() {
  var m2 = test_import("/Users/jester/src/memetalk/sugarfoot/ex2", [thisModule]);
  m2.foo(99);
}

.endcode
