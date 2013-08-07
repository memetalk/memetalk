module ex(io)
  io : memetalk/io/1.0();
{
  class Exception {
    fields: x;
    init new(x) {
      @x = x;
    }
    func throw(x) {
      var self = Exception.new(x);
      self.raise();
    }
    fun raise() {
       <primitive "exception_raise">
    }
    fun x() {
      return @x;
    }
  }

  fun r() {
    io.print("r()");
    Exception.throw(10);
    io.print("--r() DONT GO HERE");
  }

  fun i() {
    io.print("i()");
    r();
    io.print("--i(): DONT GO HERE");
  }

  fun i2() {
    io.print("i2()");
  }

  fun main() {
    var a = 2;
    try {
      io.print("trying i()");
      i();
      io.print("--try: DONT GO HERE");
    } catch(e) {
      io.print("catch!");
      return e.x() + a;
    }
    return 99;
  }
}
