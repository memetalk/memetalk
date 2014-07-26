.preamble()
.code

print: fun(arg) {
  <primitive "print">
}

assert: fun(x,desc) {
  if (!x) {
    if (desc) {
      Exception.throw("assertion failed: '" + desc + "'");
    } else {
      Exception.throw("assertion failed");
    }
  }
}

test_import: fun(filepath, args) {
  <primitive "test_import">
}

get_module_function: fun(mod, name) {
  <primitive "test_get_module_function">
}

main: fun() {
  var print = get_module_function(thisModule, :print);
  var m2 = test_import("/Users/jester/src/memetalk/sugarfoot/ex2", [print]);
  m2.foo(99);
}

.endcode
