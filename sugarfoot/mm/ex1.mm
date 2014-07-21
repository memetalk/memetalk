.preamble()
.code

print: fun(x) {
  <primitive "print">
}


main: fun() {
  var y = 10;
  var x = fun() { y };
  return x();
}

.endcode
