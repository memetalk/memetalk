module foo()
{
  fun f(fn) {
    fn();
  }

  fun main() {
    var a = 0;
    f(fun() {
      try {
        Exception.throw(10);
        assert(false, "Shouldn't execute here");
      } catch(e) {
        a = a + 1;
      }
      a = a + 1;
    });
    a = a + 1;
    assert(a == 3, "try/catch inside closure");
  }
}
