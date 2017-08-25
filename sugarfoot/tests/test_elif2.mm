.license
.endlicense

.preamble(test)

.code


cond: fun(a, b) {
 return a == 4;
}

f: fun(a, b) {
}

g: fun(a) {
}

main: fun() {
  var idx = null;
  if (cond(1, 2)) {
    idx = f(2, 3);
    f(:a, 2);
  } elif (cond(4, 5) or cond(6, 7)) {
    idx = g(6);
    f(:b, 0);
    f(:c, idx);
    f(:d, 0);
    //jumps here used to go out of bounds and segfault
  } else {
    idx = g(7);
    f(:e, 0);
    f(:f, idx);
    f(:g, 0);
  }
}
.endcode<
