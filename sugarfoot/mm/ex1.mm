.preamble()
.code

class X
  fields: a,b;
  init new: fun(a) {
    @a = a;
  }
  instance_method mya: fun() {
    return @a;
  }
  instance_method ab: fun() {
    <primitive "x_ab">
  }
  class_method c: fun(a, b) {
    <primitive "x_c">
  }
end

main: fun() {
  var x =  X.new(99);
  return x.mya();
}

.endcode
