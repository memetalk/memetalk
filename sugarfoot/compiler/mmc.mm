.preamble()
.code

FRAME_TYPE_OBJECT: 900;
FRAME_TYPE_BVAR_OBJECT: 901; // len in bytes (symbols/strings)
FRAME_TYPE_LIST_OBJECT: 902;
FRAME_TYPE_DVAR_OBJECT: 903; // len in words * 2
FRAME_TYPE_LITERAL_FRAME: 904;
FRAME_TYPE_BYTECODE_FRAME: 905;
FRAME_TYPE_EXCEPTIONS_FRAME: 906;
FRAME_TYPE_ELEMENTS: 907;

EXCEPTION_FRAME_SIZE: 3;

SEP: "_";

behavior_label: fun(name) {
 return name + SEP + "Behavior";
}

cclass_label: fun(name) {
  return name + SEP + "CompiledClass";
}

class_label: fun(name) {
  return name; // # + SEP + "Class"
}

closure_name: fun(counter) {
    return "<anonymous " + counter.toString + ">";
}

cfun_label: fun(counter, owner_label, name, is_method) {
  if (is_method) {
    // create unique label, so we can have an instance method with
    // the same name as a class method.
    return counter.toString + "_" + owner_label + SEP + name + SEP + "CompiledFunction";
  } else {
    return owner_label + SEP + name + SEP + "CompiledFunction";
  }
}

fun_label: fun(cfun_label) {
  return cfun_label + SEP + "Function";
}
cmod_label: fun(name) {
    return name + SEP + "CompiledModule";
}

mod_label: fun(cmod_name) {
    return cmod_name + SEP + "Module";
}
.endcode
