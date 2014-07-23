.preamble()
.code

print: fun(arg) {
  <primitive "print">
}


main: fun() {
  try {
    Exception.throw("assertion failed");
  } catch(e) {
    print("oi");
  }
  return 1;
}

.endcode
