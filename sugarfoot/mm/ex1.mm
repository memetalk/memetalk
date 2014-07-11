.preamble()
.code

class X
  fields: a,b;
  init new: fun(a) {
    <primitive "x_new">
  }
  instance_method ab: fun() {
    <primitive "x_ab">
  }
  class_method c: fun(a, b) {
    return b;
  }
end

main: fun() {
  return X.new(99);
}

.endcode
