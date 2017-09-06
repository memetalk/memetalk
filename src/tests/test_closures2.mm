meme foo
requires test

main: fun() {
  var z = fun(k) { k + 1 };
  test.assertEqual(x(z), 10, "testing closure parameters");
}

x: fun(f) {
  return f(9);
}
