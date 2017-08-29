.license
.endlicense

.preamble(test)

.code

// -- module functions --

main: fun() {
  var z = fun() {  };
  test.assertEqual(z(), null, "testing empty closure");
}

// -- module classes --

.endcode
