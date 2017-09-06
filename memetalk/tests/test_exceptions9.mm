meme central:memescript/compiler
requires test

// -- module functions --


g: fun(x) {
  try {
    x.append(900);
    test.assert(false, "main: shouldn't execute here");
  } catch(e) {
    return 10;
  }
  return 11;
}

f: fun() {
  return g(1);
}

main: fun() {
  test.assertEqual(f(), 10, "f() != 10");
}
