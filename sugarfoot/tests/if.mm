.license
.endlicense

.preamble(test)

.code

// -- module functions --

main: fun() {
  var a = 0;
  if (1 == 10) {
    test.assert(false, "Shouldn't execute this");
  }

  if (a == 0) {
    test.assert(true, "a == 0");
  }

  if (1 == 10) {
    test.assert(false, "Shouldn't execute this");
  } else {
    a = 1;
  }
  test.assert(a == 1, "Simple if");
}

// -- module classes --


.end
