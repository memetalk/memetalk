.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var x = X.new;
  var klass = get_compiled_class(X);
  klass.removeMethod("bar", :instance_method);
  try {
    x.bar();
    assert(false, "Shouldn't be here");
  } catch(e) {
    assert(e, "x.bar does not exist anymore");
  }
  return 1;
}

// -- module classes --

class X
fields: ;
init new: fun() {
}

instance_method bar: fun() {
  return 42;
}

end //remove_instance_method:X


.end
