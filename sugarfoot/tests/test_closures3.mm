.license
.endlicense

.preamble(test)

.code

// -- module functions --

main: fun() {
  var f = fun() { 99 };
  var z = fun(k) { f() + 1 + k };
  test.assertEqual(x(z), 109, "accessing closure environment");
}

x: fun(f) {
  return f(9);
}

// -- module classes --


.end
