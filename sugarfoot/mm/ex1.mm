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
    return a;
  }
end

main: fun() {
  return X.c(2);
}

.endcode
