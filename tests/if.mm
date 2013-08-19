.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var a = 0;
  if (1 == 10) {
    assert(false, "Shouldn't execute this");
  } else {
    a = 1;
  }
  assert(a == 1, "Simple if");
}

// -- module classes --


.end
