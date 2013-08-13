module foo()
{
  class X {
    init new: fun() {
    }
    instance_method y: fun(fn) {
      fn(1);
    }
    instance_method x: fun(fn) {
      fn(2);
    }
  }
  main: fun() {
    var res = 0;
    var e = X.new();
    e.x(fun(m) {
      if (true) {
        e.y(fun(n) {
          res = m + n;
        });
      }
    });
    assert(res == 3, "if inside nested closures triggered by methods");
  }
}
