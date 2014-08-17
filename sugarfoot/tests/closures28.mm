.preamble(test)
.code

class T
fields: x;
init new: fun() {}
instance_method foo: fun() {
  var z = fun() {};
  @x = 10;
  return @x;
}
instance_method bar: fun() {
  @x = 11;
  var z = fun() { @x };
  return z();
}
end

main: fun() {
  test.assert(T.new.foo == 10, "pop_field returning field from instance");
  test.assert(T.new.bar == 11, "closure returning field from instance");
}
.end
