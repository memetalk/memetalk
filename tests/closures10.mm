module ex()

{
  fun print(arg) {
    <primitive "io_print">
  }

  fun foo(fn) {
    print("executing foo fn");
    fn();
  }

  fun bar(fn) {
    print("executing bar fn");
    fn();
  }

  fun main() {
    foo(fun() {
      bar(fun() {
        print("inner");
      });
    });
    return null;
  }
}
