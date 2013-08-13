module memetest(io)
io : memetalk/io/1.0();
{
  _test_files: fun() {
    <primitive "test_files">
  }

  _test_import: fun(filepath) {
    <primitive "test_import">
  }

  assert: fun(x,desc) {
    if (!x) {
      if (desc) {
        Exception.throw("assertion failed: '" + desc + "'");
      } else {
        Exception.throw("assertion failed");
      }
    }
  }

  main: fun() {
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
