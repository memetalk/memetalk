module ex(io)
  io : memetalk/io/1.0();
{

  fun f(fn) {
    fn();
  }

  fun main() {
    f(fun() {
      try {
        io.print("init");
        Exception.throw(10);
        io.print("NOT HERE");
      } catch(e) {
        io.print("catch");
      }
      io.print("last");
      return 10;
    });
    return 1;
  }
}
