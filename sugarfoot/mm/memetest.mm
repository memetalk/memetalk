.license
.endlicense

.preamble(io)
 io: meme:io;
.code

print: fun(arg) {
  <primitive "io_print">
}

//outside of Exception hierarchy, so
//the catch blocks of exception tests won't suppress it
class AssertionException
  fields: message;
  init new: fun(message) {
    @message = message;
  }
  instance_method message: fun() {
    return @message;
  }
  instance_method throw: fun() {
    <primitive "exception_throw">
  }
  instance_method message: fun() {
    return @message;
  }
  instance_method toString: fun() {
    return this.message.toString();
  }
  class_method throw: fun(msg) {
    this.new(msg).throw;
  }
end

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
      AssertionException.throw("assertion failed: '" + desc + "'");
    } else {
      AssertionException.throw("assertion failed");
    }
  }
}
class_method do_test: fun(mmc_test_file) {
  try {
    var m = this.test_import(mmc_test_file, [this]);
    m.main();
    io.print("test:passed " + mmc_test_file);
  } catch(AssertionException e) {
    io.print(e.message + " on " + mmc_test_file);
    Exception.throw("test:interrupted");
  }
}
class_method start: fun() {
  this.test_files().each(fun(mmc_test_file) {
    this.do_test(mmc_test_file)
  });
}
end

main: fun() {
  Test.start();
  return 0;
}
.end
