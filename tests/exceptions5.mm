module foo()
{
  r: fun(fn) {
    fn();
    Exception.throw("xx");
    fn();
  }

  b: fun(fn) {
    fn();
    try {
      fn();
      r(fn);
      fn();
    } catch(e) {
      fn();
    }
    fn();
    Exception.throw("yy");
    fn();
  }

  a: fun(fn) {
    fn();
    try {
      fn();
      b(fn);
      fn();
    } catch(e) {
      fn();
    }
    fn();
  }

  main: fun() {
    var x = 0;
    var fn = fun() { x = x + 1; };
    a(fn);
    assert(x == 9, "Multiple try/catches");
  }
}
