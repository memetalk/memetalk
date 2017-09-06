meme foo
requires test

main: fun() {
  var i = 1;
  while (i < 10) {
    i = i + 1;
  }
  test.assertEqual(i, 10, "i != 10");
}
