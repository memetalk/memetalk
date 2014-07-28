.license
.endlicense

.preamble(io)
 io: meme:io;
.code

print: fun(arg) {
  <primitive "io_print">
}

class Test
class_method test_files: fun() {
  <primitive "test_files">
}

class_method test_import: fun(filepath, args) {
  <primitive "test_import">
}

class_method assert: fun(x,desc) {
  if (!x) {
    if (desc) {
      Exception.throw("assertion failed: '" + desc + "'");
    } else {
      Exception.throw("assertion failed");
    }
  }
}
class_method start: fun() {
  this.test_files().each(fun(mmc_test_file) {
    io.print("test:loading " + mmc_test_file);
    try {
      var m = this.test_import(mmc_test_file, [this]);
      m.main();
    } catch(e) {
      io.print(e.message + " on " + mmc_test_file);
      Exception.throw("test:interrupted");
    }
  });
}
end

main: fun() {
  Test.start();
  return 0;
}
.end
