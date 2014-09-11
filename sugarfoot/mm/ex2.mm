.preamble(io)
  io: meme:io;
.code

g: fun() {
  //Exception.throw("test");
  return 99;
}

bar: fun() {
  return g();
}

foo: fun() {
  <primitive "test_catch_exception">
}

f: fun() {
  //return foo();
  return g() + 1;
}


main: fun() {
  debug();
  f();
  //bla
  var x = 10;
  io.print(x);
  x = x + 2;
  return x + 3 - 4;
}


.endcode
