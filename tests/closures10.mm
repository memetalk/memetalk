.license
.endlicense

.preamble()

.code

// -- module functions --

bar: fun(fn) {
  fn(7);
}

foo: fun(fn) {
  fn(3);
}

main: fun() {
  foo(fun(x) {
    bar(fun(y) {
      assert(x + y == 10, "Passing nested closures with parameters");
    });
  });
}

// -- module classes --


.end
