meme central:memescript/compiler
requires test

// -- module functions --

main: fun() {
  var z = fun() {  };
  test.assertEqual(z(), null, "testing empty closure");
}

// -- module classes --
