.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var z = fun(k) { k + 1 };
  assert(z(10) == 11, "testing closure return value");
}

// -- module classes --


.end
