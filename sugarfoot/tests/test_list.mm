.license
.endlicense

.preamble(test)

.code

// -- module functions --


main: fun() {
  test.assertEqual([3,4,5].range(0, 2), [3,4], "range 1");
  test.assertEqual([3,4,5,6].range(1, -1), [4,5], "range 2");
}

.endcode
