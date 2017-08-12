.preamble(test)
.code

main: fun() {
  var z = fun(k) { k + 1 };
  test.assert(x(z) == 10, "testing closure parameters");
}

x: fun(f) {
  return f(9);
}

.end
