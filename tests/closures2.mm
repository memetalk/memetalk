.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var z = fun(k) { k + 1 };
  assert(x(z) == 10, "testing closure parameters");
}

x: fun(f) {
  return f(9);
}

// -- module classes --


.end
