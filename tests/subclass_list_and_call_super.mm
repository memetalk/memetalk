.preamble()
.code

class Input < List
init new: fun(data) {
  super.new(data);
}
instance_method rest: fun() {
  return Input.new(super());
}
end

main: fun() {
  var i = Input.new([5,6,8]).rest;
  assert(i.first == 6, "Testing super send");
  assert(i.rest.first == 8, "Testing super send2");
}
.endcode
