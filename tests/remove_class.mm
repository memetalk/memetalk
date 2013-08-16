module foo() {

  class X {
    init new: fun() {
    }
  }

  main: fun() {
    var cmod = get_compiled_module(thisModule);
    cmod.removeClass("X");
    try {
      var x = X;
      assert(false, "Shouldn't be here");
    } catch(e) {
      assert(true, "Class X removed");
    }
  }
}
