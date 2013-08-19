.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var klass = get_compiled_class(X);
  var x = X.new(99,98);
  klass.setFields(['x','z']);
  assert(x.x == 99, "Field stays the same");
  try {
    x.y();
    assert(false, "Shouldn't be here");
  } catch(e) {
    assert(true, "'y' is no longer a field");
  }
  assert(x.z == null, "'z' is a new field");
}

// -- module classes --

class X
fields: x, y;
init new: fun(x,y) {
  @x = x;
  @y = y;
}

instance_method x: fun() {
  return @x;
}

instance_method y: fun() {
  return @y;
}

instance_method z: fun() {
  return @z;
}

end //edit_fields:X


.end
