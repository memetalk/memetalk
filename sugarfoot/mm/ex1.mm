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


main: fun() {
  // try {
  //   assert(false, "oi");
  // } catch(Z e) {
  //   print("zaz");
  // }
  try {
    return ZZ.throw("oi");
  } catch(KK e) {
    print("zaz");
  }
  return 1;
}

.endcode
