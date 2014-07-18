.preamble()
.code

class Exception
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
  class_method throw: fun(msg) {
    Exception.new(msg).throw;
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
