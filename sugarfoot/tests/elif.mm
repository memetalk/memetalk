.license
.endlicense

.preamble(test)

.code

// -- module functions --

main: fun() {
  var a = 0;
  if (a == 0) {
    test.assert(true, "a == 0");
    a = 1;
  } elif (a == 1) {
    test.assert(false, "Shouldn't execute this");
  }

  if (a == 1) {
    test.assert(true, "a == 1");
    a = 2;
  } elif (a == 1) {
    test.assert(false, "Shouldn't execute this");
  } else {
    test.assert(false, "Shouldn't execute this");
  }

  if (a == 1) {
    test.assert(false, "Shouldn't execute this");
  } elif (a == 1) {
    test.assert(false, "Shouldn't execute this");
  } else {
    test.assert(true, "a == 2");
    a = 3;
  }
  test.assert(a == 3, "a == 3");
}

// -- module classes --


.end
