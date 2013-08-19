.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var fn = X.x;
  fn.compiledFunction.setCode("fun(a) { return a + 10; }");
  assert(X.x(10) == 20, "Changing class method");
}

// -- module classes --

class X
fields: ;
class_method x: fun() {
  return thisContext;
}

end //closures24:X


.end
