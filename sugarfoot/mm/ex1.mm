.preamble()
.code

main: fun() {
  var x  = 1;
  try {
    Exception.throw(1);
  } catch(e) {
    Exception.throw(2);
  }
  return x;
}

.endcode
