module foo() {

  fun get_process() {
    <primitive "vmprocess">
  }

  fun print(arg) {
    <primitive "io_print">
  }

  fun bla(p) {
    var l = 10;
    var proc = get_process();
    var stackFrames = proc.stackFrames();
    stackFrames.each(fun(frame) {
      print(frame.modulePointer());
      print(frame.contextPointer());
      print(frame.receiverPointer());
      print(frame.environmentPointer());
      print(frame.localVars());
    });
  }

  fun main() {
    var ll = "oi";
    bla(99);
    return 0;
  }
}
