module foo() {
  main: fun() {
    var z = fun(k) { k + 1 };
    assert(z(10) == 11, "testing closure return value");
  }
}
