.license
.endlicense

.preamble(test)

.code

// -- module functions --

main: fun() {
  var a = 0;
  try {
    Exception.throw("msg");
  } catch(Exception e) {
    a = a + 1;
  }
  a = a + 1;
  test.assert(a == 2, "Core Exception class");
}

// -- module classes --


.end
