.license
.endlicense

.preamble()

.code

// -- module functions --

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

// -- module classes --

class X
fields: ;
init new: fun() {
}

end //remove_class:X


.end
