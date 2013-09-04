.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var z = fun() {  };
  assert(z() == null, "testing empty closure");
}

// -- module classes --

.end
