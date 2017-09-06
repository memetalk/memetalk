meme foo
requires test

f: fun() {
  [2].each(fun(_, y) {
    [1].each(fun(_, x) {
        ^ 100;
      });
  });
  return 99;
}
main: fun() {
  test.assertEqual(f(), 100, "");
}
