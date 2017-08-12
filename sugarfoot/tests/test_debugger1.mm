.license
.endlicense

.preamble(test, io, qt)
 io : meme:io;
 qt : meme:qt;
.code

f: fun() {
   return 10;
}

main: fun() {
  set_debugger_module(thisModule);
  dbg();
  var a = 1 + 2;
  return f();
}

// module: ex1/main@[15, 14, 15, 15]
// module: ex1/main@[15, 14, 15, 15]
// module: ex1/main@[15, 10, 15, 11]
// module: ex1/main@[15, 10, 15, 15]
// module: ex1/main@[15, 2, 15, 15]
// module: ex1/main@[16, 9, 16, 12]
// module: ex1/f@[9, 10, 9, 12]

class Tester
fields: process;
init new: fun(proc) {
  @process = proc;
}
instance_method step0: fun() {
  test.assertLocation(@process, "test_debugger1/main", [15, 14, 15, 15]);
  @process.stepInto();  // step on POP for dbg() value.
}

instance_method step1: fun() {
  test.assertLocation(@process, "test_debugger1/main", [15, 14, 15, 15]);
  @process.stepInto(); //step on PUSH 2
}

instance_method step2: fun() {
  test.assertLocation(@process, "test_debugger1/main", [15, 10, 15, 11]);
  @process.stepInto(); //step on PUSH 1
}

instance_method step3: fun() {
  test.assertLocation(@process, "test_debugger1/main", [15, 10, 15, 15]);
  @process.stepInto(); //step on 1 + 2
}

instance_method step4: fun() {
  test.assertLocation(@process, "test_debugger1/main", [15, 2, 15, 15]);
  @process.stepInto(); //step on var a = 1 + 2
}

instance_method step5: fun() {
  test.assertLocation(@process, "test_debugger1/main", [16, 9, 16, 12]);
  @process.stepInto(); //step on f();
}

instance_method step6: fun() {
  test.assertLocation(@process, "test_debugger1/f", [9, 10, 9, 12]);
  @process.stepInto(); //step on 10
}

instance_method step7: fun() {
  test.assertLocation(@process, "test_debugger1/f", [9, 3, 9, 12]);
  @process.stepInto(); //step on return 10
}

instance_method step8: fun() {
  test.assertLocation(@process, "test_debugger1/main", [16, 2, 16, 12]); // return f()
  @process.detach_debugger();
}
end

debug: fun(proc) {
  var app = qt.QApplication.new(); //in case target process didn't started it
  test.set_debugger_tests(Tester.new(proc), 9);
  return test;
}
.end
