.preamble()
.code

class Exception
  class_method throw: fun(x) {
    <primitive "exception_throw">
  }
end

class F < Exception
end

print: fun(x) {
  <primitive "print">
}


foo: fun() {
  F.throw(99);
  print("FOO");
}

main: fun() {
  print(" BEFORE TRY");
  try {
    foo();
  } catch(Exception e) {
    print("ON CATCH");
  }
  print("OUT OF TRY/CATCH");
}

.endcode
