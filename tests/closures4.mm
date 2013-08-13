module foo() {

  x: fun(g) {
    return g()(10);
  }

  main: fun() {
    var f = fun() {
      var y = 10;
      return fun(x) { x + y };
    };
    var k = x(f);
    assert(k == 20, "manipulating returned closure");
  }
}
