.license
.endlicense

.preamble()

.code

// -- module functions --

main: fun() {
  var cmod = get_compiled_module(thisModule);
  var klass = cmod.newClass("X", "Object");

  var code = "fun() { return 42; }";
  var cfun = CompiledFunction.newTopLevel("bar", code, klass, :class_method);
  klass.addMethod(cfun, :class_method);
  assert(X.bar == 42, "Creating class and method");
  assert(cmod.compiled_classes["X"] == klass, "Checking if the class was added to the dict");
}

// -- module classes --


.end
