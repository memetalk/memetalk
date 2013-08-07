module ex(io)
  io : memetalk/io/1.0();
{

  fun r() {
    io.print("r:init");
    Exception.throw("xx");
    io.print("r:last");
  }

  fun b() {
    try {
      io.print("b:init");
      r();
      io.print("b:after-r");
    } catch(e) {
      io.print("b:catch " + e.value());
    }
    Exception.throw("yy");
    io.print("b:last");
  }

  fun a() {
    try {
      io.print("a:init");
      b();
      io.print("a:after-b");
    } catch(e) {
      io.print("a:catch " + e.value());
    }
    io.print("a:last");
  }

  fun main() {
    a();
  }
}
