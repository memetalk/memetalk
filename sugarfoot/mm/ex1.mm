.preamble()
.code

class X
  fields: a,b;
  init new: fun() {
    <primitive "x_new">
  }
  instance_method ab: fun() {
    <primitive "x_ab">
  }
  class_method c: fun(a) {
    <primitive "x_c">
  }
end

main: fun() {
  <primitive "main">
}

.endcode
