.license
.endlicense

.preamble(test)

.code

f: fun() {
  [2].each(fun(y) {
    [1].each(fun(x) {
        ^ 100;
      });
  });
  return 99;
}
main: fun() {
  test.assert(f() == 100, "");
}

.endcode
