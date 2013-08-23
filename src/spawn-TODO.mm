.preamble(io)
io: memetalk/io/1.0();
.code

bar: fun() {
  return 50;
}

main: fun() {
  io.print("ex1: main");
  VMProcess.spawn.exec_module(get_compiled_module(thisModule).name, 'bar', []);
  return 99;
}
.endcode
