.preamble(io)
  io: meme:io;
.code

g: fun() {
  //Exception.throw("test");
  var x = 1;
  return x;
}

bar: fun() {
  return g();
}

foo: fun() {
  <primitive "test_catch_exception">
}

f: fun() {
  //return foo();
  var z = 2;
  return g() + 3;
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
