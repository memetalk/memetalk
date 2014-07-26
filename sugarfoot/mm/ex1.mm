.preamble()
.code

class X
class_method print: fun(arg) {
  <primitive "print">
}
end

// assert: fun(x,desc) {
//   if (!x) {
//     if (desc) {
//       Z.throw("assertion failed: '" + desc + "'");
//     } else {
//       Z.throw("assertion failed");
//     }
//   }
// }

test_import: fun(filepath, args) {
  <primitive "test_import">
}

main: fun() {
  var m2 = test_import("/Users/jester/src/memetalk/sugarfoot/ex2", [X]);
  m2.foo(99);
}

.endcode
