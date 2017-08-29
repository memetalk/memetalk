.license
.endlicense

.preamble(test)

.code

// -- module functions --

i: fun(fn) {
  fn(1);
  r(fn);
  test.assert(false, "i(): shouldn't execute here");
}

main: fun() {
  var a = 0;
  var fn = fun(x) { a = a + x; };
  try {
    i(fn);
    test.assert(false, "main: shouldn't execute here");
  } catch(MyException e) {
    test.assert(e.x() + a == 12, "try/catch and exception flow with custom exception class");
  }
}

r: fun(fn) {
  fn(1);
  MyException.throw(10);
  test.assert(false, "r(): shouldn't execute here");
}

// -- module classes --

class MyException
fields: x;
init new: fun(x) {
  @x = x;
}

instance_method throw: fun() {
   <primitive "exception_throw">
}

instance_method x: fun() {
  return @x;
}

class_method throw: fun(x) {
  var self = MyException.new(x);
  self.throw();
}

end //exceptions1:MyException


.endcode
