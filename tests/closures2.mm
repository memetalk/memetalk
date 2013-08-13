module foo() {

  x: fun(f) {
    return f(9);
  }

  main: fun() {
    var z = fun(k) { k + 1 };
    assert(x(z) == 10, "testing closure parameters");
  }
}
