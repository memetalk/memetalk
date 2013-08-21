.preamble()
.code

class Input < List
init new: fun(data) {
  super.new(data);
}
instance_method test: fun() {
  return this.rest.map(fun(x) { x.toString }).join(":");
}
end

main: fun() {
  assert("5-" + Input.new([5,6,8]).test == "5-6:8", "subclassing List");
}
.endcode
