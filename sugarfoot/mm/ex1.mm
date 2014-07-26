.preamble()
.code

print: fun(arg) {
  <primitive "print">
}

class ZZ < Exception
end

class KK < ZZ
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

test_import: fun(filepath) {
  <primitive "test_import">
}

main: fun() {
  return test_import("/Users/jester/src/memetalk/sugarfoot/ex2");
}

.endcode
