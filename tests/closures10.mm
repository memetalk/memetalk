module foo()
{
  foo: fun(fn) {
    fn(3);
  }

  bar: fun(fn) {
    fn(7);
  }

  main: fun() {
    foo(fun(x) {
      bar(fun(y) {
        assert(x + y == 10, "Passing nested closures with parameters");
      });
    });
  }
}
