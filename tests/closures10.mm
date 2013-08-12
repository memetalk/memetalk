module foo()
{
  fun foo(fn) {
    fn(3);
  }

  fun bar(fn) {
    fn(7);
  }

  fun main() {
    foo(fun(x) {
      bar(fun(y) {
        assert(x + y == 10, "Passing nested closures with parameters");
      });
    });
  }
}
