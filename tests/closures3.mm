module foo() {

  x: fun(f) {
    return f(9);
  }

  main: fun() {
    var f = fun() { 99 };
    var z = fun(k) { f() + 1 + k };
    assert(x(z) == 109, "accessing closure environment");
  }
}
