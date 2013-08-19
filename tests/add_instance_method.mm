.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var x = X.new;
  var cmod = get_compiled_module(thisModule);
  var code = "fun() { return 42; }";
  var klass = get_compiled_class(X);
  var cfun = CompiledFunction.newTopLevel("bar", code, klass, :instance_method);
  klass.addMethod(cfun, :instance_method);
  assert(x.bar == 42, "Adding instance method");
}

// -- module classes --

class X
fields: ;
init new: fun() {
}

end //add_instance_method:X


.end
