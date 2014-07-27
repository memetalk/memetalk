.preamble(ex1)
  [X] <= ex1;
.code

class Z < X
class_method ha: fun() {
  this.print(888);
  return 7;
}
end

foo: fun() {
  return Z.ha();
}
.endcode
