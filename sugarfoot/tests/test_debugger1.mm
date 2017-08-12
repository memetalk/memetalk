.license
.endlicense

.preamble(test, io, qt)
 io : meme:io;
 qt : meme:qt;
.code

main: fun() {
  set_debugger_module(thisModule);
  dbg();
  var a = 1 + 1;
}

step1: fun(proc) {
  test.assertLocation(proc, "test_debugger1/main", [11, 14, 11, 15]);
  proc.stepInto();
}

step2: fun(proc) {
  test.assertLocation(proc, "test_debugger1/main", [11, 14, 11, 15]);
  proc.stepInto();
}

step3: fun(proc) {
  test.assertLocation(proc, "test_debugger1/main", [11, 10, 11, 11]);
  proc.detach_debugger();
}

debug: fun(proc) {
  var app = qt.QApplication.new(); //in case target process didn't started it
  test.set_debugger_tests([
    fun() { step1(proc) },
    fun() { step2(proc) },
    fun() { step3(proc) }]);
  return test;
}
.end
