meme central:memescript/compiler
requires test

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
      test.assertEqual(x + y, 10, "Passing nested closures with parameters");
    });
  });
}

// -- module classes --
