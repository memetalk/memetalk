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

g: fun(c) {
  return c;
}

f: fun(a,b) {
  // return g(a);
  return b;
//  return b;
}

main: fun() {
 return f(99, 98);
}

.endcode
