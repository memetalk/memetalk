.preamble(io)
  io: meme:io;
.code

g: fun() {
  Exception.throw("test");
  var x = 1;
  return x;
}

f: fun() {
  //return foo();
  var z = 2;
  var k = g() + 3 + z;
  return k + 1;
}

bar: fun() {
  return f();
}

foo: fun() {
  <primitive "test_catch_exception">
}


class Z
  fields: a;
  init new: fun() {
    @a = 99;
  }
end

main: fun() {
  var x = Z.new;
  debug();
  var fn = fun() {
    var xx = 1;
    return foo();
  };
  return fn();
}


.endcode
