.preamble()
.code

main: fun() {
  var x = 0;
  [1,1,1].each(fun(y) {
    x = x + y;
    return x;
  });

  assert(x == 3, "local return, x == 3");

  fun() {
    [4,3,2].each(fun(y) {
      assert(y == 4, "first iteration: x == 4");
        ^ y;
    });
  }();

  assert(false, "shouldn't be here");
}

.endcode
