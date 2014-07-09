#include "mmobj.hpp"
#include "defs.hpp"
#include "core_image.hpp"
#include <cstdlib>
#include "report.hpp"
#include <assert.h>


MMObj::MMObj(CoreImage* core)
  : _core_image(core) {
}

oop MMObj::mm_module_new(int num_classes, oop delegate) {
  oop imodule = (oop) malloc(sizeof(word) * (3 + num_classes)); //3: vt, delegate, dict

  ((word**) imodule)[0] = imodule; //imodule[vt] = imodule
  ((word**) imodule)[1] = delegate; // imodule[delegate]
  return imodule;
}

oop MMObj::mm_compiled_module_classes(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[6];
}

oop MMObj::mm_compiled_module_functions(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[5];
}

oop MMObj::mm_object_new() {
  oop obj = (oop) malloc(sizeof(word) * 2); // vt, delegate

  ((word**) obj)[0] = _core_image->get_prime("Object");
  ((word**) obj)[1] = 0;
  return obj;
}

oop MMObj::mm_list_new_empty() {
  oop obj = (oop) malloc(sizeof(word) * 3); // vt, delegate, size

  ((word**) obj)[0] = _core_image->get_prime("List");
  ((word**) obj)[1] = mm_object_new();
  ((word**) obj)[2] = 0;
  return obj;
}

oop MMObj::mm_dictionary_new(int num_entries) {
  int basic = 3; //vt, delegate, size
  int payload = num_entries * 2;
  oop obj = (oop) malloc(sizeof(word) * (basic + payload));

  ((word**) obj)[0] = _core_image->get_prime("Dictionary");
  ((word**) obj)[1] = mm_object_new();
  ((word*) obj)[2] = num_entries;
  return obj;
}

number MMObj::mm_dictionary_size(oop dict) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));
  return (number) ((word*)dict)[2];
}


bool MMObj::mm_dictionary_has_key(oop dict, oop key) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));

  number size = mm_dictionary_size(dict);
  debug() << "Dict size " << size << std::endl;
  for(int i = 0; i < size; i++) {
    if (mm_dictionary_entry_key(dict, i) == key) {
      return true;
    }
  }
  return false;
}

oop MMObj::mm_dictionary_get(oop dict, oop key) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));

  number size = mm_dictionary_size(dict);
  for(int i = 0; i < size; i++) {
    if (mm_dictionary_entry_key(dict, i) == key) {
      return mm_dictionary_entry_value(dict, i);
    }
  }
  return NULL;
}


oop MMObj::mm_dictionary_entry_key(oop dict, int idx) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));
  //0: vt
  //1: delegate
  //2: size
  return (word*) dict[3 + (idx * 2)];
}

oop MMObj::mm_dictionary_entry_value(oop dict, int idx) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));
  //0: vt
  //1: delegate
  //2: size
  return (word*) dict[3 + ((idx * 2)+1)];
}

void MMObj::mm_dictionary_set(oop dict, int idx, oop key, oop value) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));

  * (oop*) &dict[3 + (idx * 2)] = key;
  * (oop*) &dict[3 + ((idx * 2)+1)] = value;
}

void MMObj::mm_module_set_dictionary(oop imodule, oop imod_dict) {
  assert( *(oop*) imod_dict == _core_image->get_prime("Dictionary"));
  ((word**) imodule)[2] = imod_dict;
}

char* MMObj::mm_string_cstr(oop str) {
  assert( *(oop*) str == _core_image->get_prime("String"));
  //0: vt
  //1: delegate
  //2: size
  //3: <str> ...
  return (char*) &(str[3]);
}

oop MMObj::mm_function_from_cfunction(oop cfun, oop imod) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));

  oop fun = (oop) malloc(sizeof(word) * 4); //vt, delegate, cfun, module

  * (oop*) fun = _core_image->get_prime("Function");
  * (oop*) &fun[1] = mm_object_new();
  * (oop*) &fun[2] = cfun;
  * (oop*) &fun[3] = imod;
  return fun;
}

oop MMObj::mm_function_get_module(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  return (oop) ((oop*)fun)[3];
}

oop MMObj::mm_function_get_cfun(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  return (oop) ((oop*)fun)[2];
}

bool MMObj::mm_function_is_prim(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  oop cfun = mm_function_get_cfun(fun);
  return (oop) ((oop*)cfun)[5];
}

oop MMObj::mm_function_get_prim_name(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  oop cfun = mm_function_get_cfun(fun);
  return (oop) ((oop*)cfun)[6];
}


bytecode* MMObj::mm_function_get_code(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_code(cfun);
}

number MMObj::mm_function_get_code_size(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_code_size(cfun);
}

oop MMObj::mm_function_get_literal_by_index(oop fun, int idx) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_literal_by_index(cfun, idx);
}

number MMObj::mm_function_get_num_params(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_num_params(cfun);
}

number MMObj::mm_function_get_num_locals(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_num_locals(cfun);
}

number MMObj::mm_compiled_function_get_num_params(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[10];
}

number MMObj::mm_compiled_function_get_num_locals(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[11];
}

number MMObj::mm_compiled_function_get_literal_frame_size(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[12];
}

oop MMObj::mm_compiled_function_get_literal_by_index(oop cfun, int idx) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  oop* literal_frame =  ((oop**)cfun)[13];
  assert( (idx * WSIZE) < mm_compiled_function_get_literal_frame_size(cfun));
  return * (oop*) literal_frame[idx];
}

number MMObj::mm_compiled_function_get_code_size(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[14];
}

bytecode* MMObj::mm_compiled_function_get_code(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (bytecode*) ((oop*)cfun)[15];
}

oop MMObj::mm_compiled_class_super_name(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return (oop) ((word*)cclass)[4];
}

oop MMObj::mm_compiled_class_own_methods(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return (oop) ((word*)cclass)[7];
}

oop MMObj::mm_cfuns_to_funs_dict(oop cfuns_dict, oop imod) {
  assert( *(oop*) cfuns_dict == _core_image->get_prime("Dictionary"));

  number size = mm_dictionary_size(cfuns_dict);
  oop funs_dict = mm_dictionary_new(size);
  for (int i = 0; i < size; i++) {
    oop str_name = mm_dictionary_entry_key(cfuns_dict, i);
    oop cfun = mm_dictionary_entry_value(cfuns_dict, i);
    oop fun = mm_function_from_cfunction(cfun, imod);
    mm_dictionary_set(funs_dict, i, str_name, fun);
  }
  return funs_dict;
}

oop MMObj::mm_class_behavior_new(oop super_class, oop funs_dict) {
  assert( *(oop*) funs_dict == _core_image->get_prime("Dictionary"));
  //vt: Behavior
  //delegate:
  //dict
  oop cbehavior = (oop) malloc(sizeof(word) * 3);
  * (oop*) cbehavior = _core_image->get_prime("Behavior");
  * (oop*) &cbehavior[1] = * (oop*) super_class; //super_class[vt]
  * (oop*) &cbehavior[2] = funs_dict;
  return cbehavior;
}


oop MMObj::mm_class_new(oop class_behavior, oop super_class, oop dict, oop compiled_class) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));
  assert( *(oop*) compiled_class == _core_image->get_prime("CompiledClass"));

  oop klass = (oop) malloc(sizeof(word) * 4);
  * (oop*) klass = class_behavior;
  * (oop*) &klass[1] = super_class;
  * (oop*) &klass[2] = dict;
  * (oop*) &klass[3] = compiled_class;
  return klass;
}


oop MMObj::mm_new_class_getter(oop imodule, oop cclass, oop name, int idx) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  assert( *(oop*) name == _core_image->get_prime("String"));

  oop cfun_getter = (oop) calloc(sizeof(word), 12);

  * (oop*) cfun_getter = _core_image->get_prime("CompiledFunction");
  * (oop*) &cfun_getter[1] = mm_object_new();
  * (oop*) &cfun_getter[2] = name;
  * (oop*) &cfun_getter[3] = mm_list_new_empty();
  * (word*) &cfun_getter[7] = 1;
  * (word*) &cfun_getter[8] = idx;
  * (oop*) &cfun_getter[9] = cclass;

  return mm_function_from_cfunction(cfun_getter, imodule);
}

oop MMObj::mm_symbol_new(const char* str) {
  oop symb = (oop) malloc((sizeof(word) * 3) + (strlen(str)+1));

  * (oop*) symb = _core_image->get_prime("Symbol");
  * (oop*) &symb[1] = mm_object_new();
  * (word*) &symb[2] = strlen(str);

  char* target = (char*) &symb[3];
  for (int i = 0; i <= strlen(str); i++) {
    target[i] = str[i];
  }
  return symb;
}

char* MMObj::mm_symbol_cstr(oop sym) {
  assert( *(oop*) sym == _core_image->get_prime("Symbol"));
  //0: vt
  //1: delegate
  //2: size
  //3: <str> ...
  return (char*) &(sym[3]);
}


oop MMObj::mm_behavior_get_dict(oop behavior) {
  //assert( *(oop*) behavior == _core_image->get_prime("Behavior")); -- this can also be an imodule
  return (oop) ((oop*)behavior)[2];
}


bool MMObj::mm_is_small_int(oop num) {
#if WSIZE == 8
  return (bool) ((word) num & 0x8000000000000000);
#else
  return (bool) ((word) num & 0x80000000);
#endif
}

number MMObj::mm_untag_small_int(oop num) {
#if WSIZE == 8
  return ((word) num & 0x7FFFFFFFFFFFFFFF);
#else
  return ((word) num & 0x7FFFFFFF);
#endif
}
