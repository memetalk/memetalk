module ex(io)
  io : memetalk/io/1.0();
{
  fun f() {
    io.print("init");
    Exception.throw(10);
    io.print("NOT HERE");
  }

  fun main() {
    try {
      f();
    } catch(e) {
      io.print("catch");
    }
    io.print("last");
    return 10;
  }
}
