.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  assert(X.new().bla() == 10, "Closure accessing instance field");
}

// -- module classes --

class X
fields: bla;
init new: fun() {
  var f = fun() {
    @bla = 10;
  };
  f();
}

instance_method bla: fun() {
  return @bla;
}

end //closures9:X


.end
