.preamble()
.code

print: fun(arg) {
  <primitive "print">
}


main: fun() {
 var x = [4,5,6];
  x.each(fun(z) { print(z + 1) });
  return 1;
}

.endcode
