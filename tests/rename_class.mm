.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var x = X;
  var cmod = get_compiled_module(thisModule);
  var klass = get_compiled_class(X);
  klass.rename("Y");
  try {
    var z = X;
    assert(false, "Shouldn't be here");
  } catch(e) {
    assert(x == Y, "Testing renaming of class");
    assert(Y.bar == 42, "Testing renaming of class");
  }
}

// -- module classes --

class X
fields: ;
class_method bar: fun() {
  return 42;
}

end //rename_class:X


.end
