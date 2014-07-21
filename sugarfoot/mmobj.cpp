#include "mmobj.hpp"
#include "defs.hpp"
#include "core_image.hpp"
#include <cstdlib>
#include "report.hpp"
#include "vm.hpp"
#include <assert.h>
#include "utils.hpp"

MMObj::MMObj(VM* vm, CoreImage* core)
  : _vm(vm), _core_image(core) {
}

oop MMObj::mm_module_new(int num_params, int num_classes, oop delegate) {
  oop imodule = (oop) malloc(sizeof(word) * (3 + num_classes + num_params)); //3: vt, delegate, dict

  ((word**) imodule)[0] = imodule; //imodule[vt] = imodule
  ((word**) imodule)[1] = delegate; // imodule[delegate]
  return imodule;
}

oop MMObj::mm_compiled_module_classes(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[7];
}

oop MMObj::mm_compiled_module_functions(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[6];
}

oop MMObj::mm_compiled_module_params(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[4];
}

oop MMObj::mm_compiled_module_default_params(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[5];
}


oop MMObj::mm_object_new() {
  oop obj = (oop) malloc(sizeof(word) * 2); // vt, delegate

  ((word**) obj)[0] = _core_image->get_prime("Object");
  ((word**) obj)[1] = 0;
  return obj;
}

oop MMObj::mm_object_vt(oop obj) {
  if (is_small_int(obj)) {
    return _core_image->get_prime("Number");
  } else {
    return * (oop*) obj;
  }
}

oop MMObj::mm_object_delegate(oop obj) {
  if (is_small_int(obj)) {
    return mm_object_new(); //TODO: this should probably be a dummy static object
  } else {
    return ((oop*) obj)[1];
  }
}

oop MMObj::mm_list_new_empty() {
  oop obj = (oop) malloc(sizeof(word) * 3); // vt, delegate, size

  ((word**) obj)[0] = _core_image->get_prime("List");
  ((word**) obj)[1] = mm_object_new();
  ((word**) obj)[2] = 0;
  return obj;
}

number MMObj::mm_list_size(oop list) {
  assert( *(oop*) list == _core_image->get_prime("List"));
  return (number) ((word*)list)[2];
}

oop MMObj::mm_list_entry(oop list, number idx) {
  assert( *(oop*) list == _core_image->get_prime("List"));
  number size = mm_list_size(list);
  for(int i = 0; i < size; i++) {
    if (i == idx) {
      return ((oop*)list)[idx+3];
    }
  }
  bail("list entry not found");
  return NULL;
}

number MMObj::mm_list_index_of(oop list, oop elem) {
  assert( *(oop*) list == _core_image->get_prime("List"));
  number size = mm_list_size(list);
  for(int i = 0; i < size; i++) {
    oop e = ((oop*)list)[i+3];
    if (e == elem) {
      return i;
    }
  }
  return -1;
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
  debug() << " Dict " << dict << " size " << size << " lookup: " << key << endl;

  for(int i = 0; i < size; i++) {
    oop mykey = mm_dictionary_entry_key(dict, i);
    // debug() << " {vt: " << mm_object_vt(mykey) << "} =?= " << key << endl;
    if (mykey == key) {
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

void MMObj::mm_module_set_module_argument(oop imodule, oop arg, number idx) {
  ((oop*)imodule)[idx+3] = arg; //3: vt, delegate, dict
}

oop MMObj::mm_module_entry(oop imodule, number idx) {
  return ((oop*)imodule)[idx+3]; //3: vt, delegate, dict
}



bool MMObj::mm_is_string(oop obj) {
  if (is_small_int(obj)) {
    return false;
  }
  return *(oop*) obj == _core_image->get_prime("String");
}


char* MMObj::mm_string_cstr(oop str) {
  assert( *(oop*) str == _core_image->get_prime("String"));
  //0: vt
  //1: delegate
  //2: size
  //3: <str> ...
  return (char*) &(str[3]);
}

bool MMObj::mm_is_function(oop obj) {
  return *(oop*) obj == _core_image->get_prime("Function");
}

bool MMObj::mm_is_context(oop obj) {
  return *(oop*) obj == _core_image->get_prime("Context");
}

oop MMObj::mm_context_get_env(oop ctx) {
  assert( *(oop*) ctx == _core_image->get_prime("Context"));
  return ((oop*)ctx)[4];
}


bool MMObj::mm_function_uses_env(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_uses_env(cfun);
}

oop MMObj::mm_function_from_cfunction(oop cfun, oop imod) {
  assert(*(oop*) cfun == _core_image->get_prime("CompiledFunction"));

  oop fun = (oop) malloc(sizeof(word) * 4); //vt, delegate, cfun, module

  * (oop*) fun = _core_image->get_prime("Function");
  * (oop*) &fun[1] = mm_object_new();
  * (oop*) &fun[2] = cfun;
  * (oop*) &fun[3] = imod;
  return fun;
}

oop MMObj::mm_function_get_module(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  return (oop) ((oop*)fun)[3];
}

oop MMObj::mm_function_get_cfun(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  return (oop) ((oop*)fun)[2];
}

bool MMObj::mm_function_is_prim(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_is_prim(cfun);
}

oop MMObj::mm_function_get_prim_name(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_prim_name(cfun);
}


bytecode* MMObj::mm_function_get_code(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_code(cfun);
}

number MMObj::mm_function_get_code_size(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_code_size(cfun);
}

oop MMObj::mm_function_get_literal_by_index(oop fun, int idx) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_literal_by_index(cfun, idx);
}

number MMObj::mm_function_get_num_params(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_num_params(cfun);
}

number MMObj::mm_function_get_num_locals_or_env(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_num_locals_or_env(cfun);
}

bool MMObj::mm_function_is_getter(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_is_getter(cfun);
}

number MMObj::mm_function_access_field(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_access_field(cfun);
}

bool MMObj::mm_function_is_ctor(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_is_ctor(cfun);
}

number MMObj::mm_function_exception_frames_count(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_exception_frames_count(cfun);
}

oop MMObj::mm_function_exception_frames(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_exception_frames(cfun);
}

bool MMObj::mm_compiled_function_is_ctor(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[4];
}

bool MMObj::mm_compiled_function_is_prim(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[5];
}

oop MMObj::mm_compiled_function_get_prim_name(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (oop) ((oop*)cfun)[6];
}

bool MMObj::mm_compiled_function_is_getter(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((number) ((oop*)cfun)[7]) == 1;
}

number MMObj::mm_compiled_function_access_field(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[8];
}

number MMObj::mm_compiled_function_get_num_params(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[10];
}

bool MMObj::mm_compiled_function_uses_env(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (bool) ((oop*)cfun)[11];
}

bool MMObj::mm_compiled_function_is_top_level(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (bool) ((oop*)cfun)[12];
}

oop MMObj::mm_compiled_function_outer_cfun(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[13];
}

number MMObj::mm_compiled_function_get_num_locals_or_env(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[14];
}

number MMObj::mm_compiled_function_get_literal_frame_size(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[15];
}

oop MMObj::mm_compiled_function_get_literal_by_index(oop cfun, int idx) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  oop* literal_frame =  ((oop**)cfun)[16];
  assert( (idx * WSIZE) < mm_compiled_function_get_literal_frame_size(cfun));
  return (oop) literal_frame[idx];
}

number MMObj::mm_compiled_function_get_code_size(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[17];
}

bytecode* MMObj::mm_compiled_function_get_code(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (bytecode*) ((oop*)cfun)[18];
}

number MMObj::mm_compiled_function_exception_frames_count(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[19];
}

oop MMObj::mm_compiled_function_exception_frames(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[20];
}


oop MMObj::mm_compiled_class_name(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return ((oop*)cclass)[2];
}

oop MMObj::mm_compiled_class_super_name(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return ((oop*)cclass)[3];
}

oop MMObj::mm_compiled_class_methods(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return ((oop*)cclass)[5];
}

oop MMObj::mm_compiled_class_own_methods(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return ((oop*)cclass)[6];
}



number MMObj::mm_compiled_class_num_fields(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  oop fields_list = ((oop*)cclass)[4];
  return mm_list_size(fields_list);
}

oop MMObj::mm_cfuns_to_funs_dict(oop cfuns_dict, oop imod) {
  assert( *(oop*) cfuns_dict == _core_image->get_prime("Dictionary"));

  number size = mm_dictionary_size(cfuns_dict);
  oop funs_dict = mm_dictionary_new(size);
  for (int i = 0; i < size; i++) {
    oop str_name = mm_dictionary_entry_key(cfuns_dict, i);
    char* str = mm_string_cstr(str_name);
    oop cfun = mm_dictionary_entry_value(cfuns_dict, i);
    oop fun = mm_function_from_cfunction(cfun, imod);
    mm_dictionary_set(funs_dict, i, _vm->new_symbol(str), fun);
  }
  return funs_dict;
}

oop MMObj::mm_class_behavior_new(oop super_class, oop funs_dict) {
  assert( *(oop*) funs_dict == _core_image->get_prime("Dictionary"));
  //vt: Behavior
  //delegate:
  //dict
  oop cbehavior = (oop) malloc(sizeof(word) * 4);
  * (oop*) cbehavior = _core_image->get_prime("Behavior");
  * (oop*) &cbehavior[1] = * (oop*) super_class; //super_class[vt]
  * (oop*) &cbehavior[2] = funs_dict;
  * (word*) &cbehavior[3] = (word) 256; //flag
  return cbehavior;
}


oop MMObj::mm_class_new(oop class_behavior, oop super_class, oop dict, oop compiled_class, number payload) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));
  assert( *(oop*) compiled_class == _core_image->get_prime("CompiledClass"));

  oop klass = (oop) malloc(sizeof(word) * 5);
  * (oop*) klass = class_behavior;
  * (oop*) &klass[1] = super_class;
  * (oop*) &klass[2] = dict;
  * (word*) &klass[3] = payload;
  * (oop*) &klass[4] = compiled_class;
  return klass;
}

oop MMObj::mm_class_name(oop klass) {
  oop cclass = mm_class_get_compiled_class(klass);
  return mm_compiled_class_name(cclass);
}

oop MMObj::mm_class_dict(oop klass) {
  return ((oop*)klass)[2];
}

oop MMObj::mm_class_get_compiled_class(oop klass) {
  return ((oop*)klass)[4];
}


oop MMObj::mm_new_slot_getter(oop imodule, oop owner, oop name, int idx) {
  // assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  assert( *(oop*) name == _core_image->get_prime("String"));

  oop cfun_getter = (oop) calloc(sizeof(word), 17);

  * (oop*) cfun_getter = _core_image->get_prime("CompiledFunction");
  * (oop*) &cfun_getter[1] = mm_object_new();
  * (oop*) &cfun_getter[2] = name;
  * (oop*) &cfun_getter[3] = mm_list_new_empty();
  * (word*) &cfun_getter[7] = 1;
  * (word*) &cfun_getter[8] = idx;
  * (oop*) &cfun_getter[9] = owner;

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

number MMObj::mm_behavior_size(oop behavior) {
  assert( **(oop**) behavior == _core_image->get_prime("Behavior"));
  oop num = ((oop*)behavior)[3];
  if (is_small_int(num)) {
    debug() << "WARNING: behavior size is tagged and I will untag it" << endl;
    return untag_small_int(num);
  } else {
    return (number) num;
  }
}

bool MMObj::is_subtype(oop sub_type, oop super_type) {
  debug() << "subtype? SUB " << sub_type << " " << super_type << endl;
  if (sub_type == NULL) {
    return false;
  }
  if (sub_type == super_type) {
    return true;
  } else {
    return is_subtype(mm_object_delegate(sub_type), super_type);
  }
}


oop MMObj::mm_new(oop vt, oop delegate, number payload) {
  oop obj = (oop) malloc(sizeof(word) * (2 + payload)); // vt, delegate

  ((word**) obj)[0] = vt;
  ((word**) obj)[1] = delegate;
  return obj;
}
