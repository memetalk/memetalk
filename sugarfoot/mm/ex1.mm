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
  class_method c: fun() {
    return 1;
  }
end

main: fun() {
  return X.c();
}

.endcode
