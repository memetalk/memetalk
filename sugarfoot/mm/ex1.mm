.preamble()
.code

print: fun(arg) {
  <primitive "print">
}


g: fun(k) {
  return k(2);
}

main: fun() {
  var y = 10;
  var x = fun(q) { y + q };
  return g(x);
}

.endcode
