.license
.endlicense

.preamble(test)

.code

// -- module functions --

main: fun() {
  var z = fun() {  };
  test.assert(z() == null, "testing empty closure");
}

// -- module classes --

.end
