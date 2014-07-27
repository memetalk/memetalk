.preamble()
.code

class X
class_method print: fun(arg) {
  <primitive "print">
}
end

test_import: fun(filepath, args) {
  <primitive "test_import">
}

main: fun() {
  var m2 = test_import("/Users/jester/src/memetalk/sugarfoot/ex2", [thisModule]);
  return m2.foo();
}

.endcode
