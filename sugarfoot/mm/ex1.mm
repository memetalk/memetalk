.preamble()
.code

class Exception
  class_method throw: fun() {
    <primitive "exception_throw">
  }
end

print: fun(x) {
  <primitive "print">
}



main: fun() {
  try {
    Exception.throw();
  } catch(Exception e) {
    print("oi");
  }
}

.endcode
