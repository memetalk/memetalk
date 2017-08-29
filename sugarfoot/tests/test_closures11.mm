.license
.endlicense

.preamble(test)

.code

// -- module functions --

main: fun() {
  var res = 0;
  var e = X.new();
  e.x(fun(m) {
    if (true) {
      e.y(fun(n) {
        res = m + n;
      });
    }
  });
  test.assertEqual(res, 3, "if inside nested closures triggered by methods");
}

// -- module classes --

class X
fields: ;
init new: fun() {
}

instance_method x: fun(fn) {
      fn(2);
    }

instance_method y: fun(fn) {
      fn(1);
    }

end //closures11:X


.endcode
