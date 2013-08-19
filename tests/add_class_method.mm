.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var cmod = get_compiled_module(thisModule);
  var code = "fun() { return 42; }";
  var klass = get_compiled_class(X);
  var cfun = CompiledFunction.newTopLevel("bar", code, klass, :class_method);
  klass.addMethod(cfun, :class_method);
  assert(X.bar == 42, "Adding class method");
}

// -- module classes --

class X
fields: ;
end //add_class_method:X


.end
