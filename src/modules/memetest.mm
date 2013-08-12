module memetest(io)
io : memetalk/io/1.0();
{
  fun _test_files() {
    <primitive "test_files">
  }

  fun _test_import(filepath) {
    <primitive "test_import">
  }

  fun assert(x,desc) {
    if (!x) {
      if (desc) {
        Exception.throw("assertion failed: '" + desc + "'");
      } else {
        Exception.throw("assertion failed");
      }
    }
  }

  fun main() {
    _test_files().each(fun(path) {
      io.print("test:loading " + path);
      var m = _test_import(path);
      io.print("test:executing " + path);
      try {
        m.main();
      } catch(e) {
        io.print(e.value + " on " + path);
        Exception.throw("test:interrupted");
      }
    });
    return "ok";
  }
}