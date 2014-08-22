#include "mmobj.hpp"
#include "defs.hpp"
#include "core_image.hpp"
#include <cstdlib>
#include "report.hpp"
#include "vm.hpp"
#include <assert.h>
#include "utils.hpp"

#include <map>

MMObj::MMObj(VM* vm, CoreImage* core)
  : _vm(vm), _core_image(core) {
}

oop MMObj::mm_module_new(int num_fields, oop cmod, oop delegate) {
  oop imodule = (oop) malloc(sizeof(word) * (4 + num_fields)); //4: vt, delegate, dict, cmod

  ((word**) imodule)[0] = imodule; //imodule[vt] = imodule
  ((word**) imodule)[1] = delegate; // imodule[delegate]
  //imod[dict]
  ((word**) imodule)[3] = cmod; // imodule[cmod]
  return imodule;
}

oop MMObj::mm_compiled_module_name(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[2];
}

//license 3

oop MMObj::mm_compiled_module_params(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[4];
}


oop MMObj::mm_compiled_module_default_params(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[5];
}

oop MMObj::mm_compiled_module_aliases(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[6];
}

oop MMObj::mm_compiled_module_functions(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[7];
}

oop MMObj::mm_compiled_module_classes(oop cmod) {
  assert( *(oop*) cmod == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[8];
}

oop MMObj::mm_boolean_new(number val) {
  return val ? MM_TRUE : MM_FALSE;
}

bool MMObj::mm_boolean_cbool(oop val) {
  assert(val == MM_TRUE || val == MM_FALSE);
  return val == MM_TRUE;
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
  } else if (obj == MM_TRUE) {
    return _core_image->get_prime("Boolean");
  } else if (obj == MM_FALSE) {
    return _core_image->get_prime("Boolean");
  } else if (obj == MM_NULL) {
    return _core_image->get_prime("Null");
  } else {
    return * (oop*) obj;
  }
}

oop MMObj::mm_object_delegate(oop obj) {
  if (is_small_int(obj) || (obj == MM_TRUE) || (obj == MM_FALSE) || (obj == MM_NULL)) {
    return obj; //mm_object_new(); //TODO: this should probably be a dummy static object
  } else {
    return ((oop*) obj)[1];
  }
}

oop MMObj::mm_list_new() {
  oop obj = (oop) malloc(sizeof(word) * 4); // vt, delegate, size, elements frame

  ((word**) obj)[0] = _core_image->get_prime("List");
  ((word**) obj)[1] = mm_object_new();
  ((word*)  obj)[2] = -1;
  ((std::vector<oop>**) obj)[3] = new std::vector<oop>;
  return obj;
}

number MMObj::mm_list_size(oop list) {
  assert( *(oop*) list == _core_image->get_prime("List"));
  std::vector<oop>* elements = mm_list_frame(list);
  return elements->size();
}

oop MMObj::mm_list_entry(oop list, number idx) {
  assert( *(oop*) list == _core_image->get_prime("List"));
  std::vector<oop>* elements = mm_list_frame(list);
  return (*elements)[idx];
}

std::vector<oop>* MMObj::mm_list_frame(oop list) {
  assert( *(oop*) list == _core_image->get_prime("List"));
  number size = (number) ((oop*)list)[2];
  if (size == -1) {
    return (std::vector<oop>*) ((oop*)list)[3];
  } else {
    ((word*)list)[2] = -1;
    std::vector<oop>* elements = new std::vector<oop>;
    oop frame = ((oop*)list)[3];
    for (int i = 0; i < size; i++) {
      elements->push_back(((oop*)frame)[i]);
    }
    ((std::vector<oop>**) list)[3] = elements;
    return elements;
  }
}

number MMObj::mm_list_index_of(oop list, oop elem) {
  assert( *(oop*) list == _core_image->get_prime("List"));
  std::vector<oop>* elements = mm_list_frame(list);

  int pos = std::find(elements->begin(), elements->end(), elem) - elements->begin();
  if (pos == mm_list_size(list)) {
    return -1;
  } else {
    return pos;
  }
}

void MMObj::mm_list_prepend(oop list, oop element) {
  assert( *(oop*) list == _core_image->get_prime("List"));

  std::vector<oop>* elements = mm_list_frame(list);
  elements->insert(elements->begin(), element);
}

void MMObj::mm_list_append(oop list, oop element) {
  assert( *(oop*) list == _core_image->get_prime("List"));

  std::vector<oop>* elements = mm_list_frame(list);
  elements->push_back(element);
}

oop MMObj::mm_dictionary_new() {
  int basic = 4; //vt, delegate, size, frame
  oop obj = (oop) malloc(sizeof(word) * basic);

  ((word**) obj)[0] = _core_image->get_prime("Dictionary");
  ((word**) obj)[1] = mm_object_new();
  ((word*) obj)[2] = -1;
  ((std::map<oop, oop>**) obj)[3] = new std::map<oop, oop>;
  return obj;
}

std::map<oop, oop>* MMObj::mm_dictionary_frame(oop dict) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));

  number size = (number) ((oop*)dict)[2];
  if (size == -1) {
    return (std::map<oop, oop>*) ((oop*)dict)[3];
  } else {
    ((word*)dict)[2] = -1;
    std::map<oop, oop>* elements = new std::map<oop, oop>;
    oop frame = ((oop*)dict)[3];
    for (int i = 0, j = 0; i < size; i++, j += 2) {
      (*elements)[((oop*)frame)[j]] = ((oop*)frame)[j+1];
    }
    ((std::map<oop, oop>**) dict)[3] = elements;
    return elements;
  }
}

std::map<oop,oop>::iterator MMObj::mm_dictionary_begin(oop dict) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));
  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  return elements->begin();
}

std::map<oop,oop>::iterator MMObj::mm_dictionary_end(oop dict) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));
  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  return elements->end();
}

number MMObj::mm_dictionary_size(oop dict) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));
  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  return elements->size();
}

bool MMObj::mm_dictionary_has_key(oop dict, oop key) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));

  debug() << dict << " has key " << key << " ?" << endl;
  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  for (std::map<oop, oop>::iterator it = elements->begin(); it != elements->end(); it++) {
    debug() << it->first << " =?= " << key << endl;
    if (it->first == key) {
      return true;
    }
  }
  return false;
}

oop MMObj::mm_dictionary_get(oop dict, oop key) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));

  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  return elements->at(key);
}


void MMObj::mm_dictionary_set(oop dict, oop key, oop value) {
  assert( *(oop*) dict == _core_image->get_prime("Dictionary"));

  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  (*elements)[key] = value;
}


void MMObj::mm_module_set_dictionary(oop imodule, oop imod_dict) {
  assert( *(oop*) imod_dict == _core_image->get_prime("Dictionary"));
  ((word**) imodule)[2] = imod_dict;
}

oop MMObj::mm_module_dictionary(oop imodule) {
  return ((oop*) imodule)[2];
}

void MMObj::mm_module_set_module_argument(oop imodule, oop arg, number idx) {
  ((oop*)imodule)[idx+4] = arg; //4: vt, delegate, dict, cmod
}

oop MMObj::mm_module_get_param(oop imodule, number idx) {
  return ((oop*)imodule)[idx+4]; //4: vt, delegate, dict, cmod
}

oop MMObj::mm_module_entry(oop imodule, number idx) {
  return ((oop*)imodule)[idx+4]; //4: vt, delegate, dict, cmod
}

oop MMObj::mm_module_get_cmod(oop imodule) {
  return ((oop*)imodule)[3]; //3: vt, delegate, dict, cmod
}


bool MMObj::mm_is_string(oop obj) {
  if (is_small_int(obj)) {
    return false;
  }
  return *(oop*) obj == _core_image->get_prime("String");
}


oop MMObj::mm_string_new(const char* str) {
  number payload = sizeof(oop) + strlen(str) + 1; //size, <str ...>, \0

  oop oop_str = mm_new(_core_image->get_prime("String"),
                       mm_object_new(), //assuming String < Object
                       payload); //this is going to alloc more than we need as it
                                 //is payload * sizeof(oop)
  ((oop*)oop_str)[2] = (oop) strlen(str);
  strcpy((char*)(oop_str + 3), str);
  return oop_str;
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

oop MMObj::mm_function_get_name(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_name(cfun);
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

number MMObj::mm_function_get_env_offset(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_env_offset(cfun);
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

oop MMObj::mm_function_get_owner(oop fun) {
  assert( *(oop*) fun == _core_image->get_prime("Function") ||
          *(oop*) fun == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_owner(cfun);
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

oop MMObj::mm_compiled_function_get_name(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (oop) ((oop*)cfun)[2];
}

bool MMObj::mm_compiled_function_is_getter(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((number) ((oop*)cfun)[7]) == 1;
}

number MMObj::mm_compiled_function_access_field(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[8];
}

oop MMObj::mm_compiled_function_get_owner(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[9];
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

number MMObj::mm_compiled_function_get_env_offset(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[15];
}

number MMObj::mm_compiled_function_get_literal_frame_size(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[16];
}

oop MMObj::mm_compiled_function_get_literal_by_index(oop cfun, int idx) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  oop* literal_frame =  ((oop**)cfun)[17];
  assert( (idx * WSIZE) < mm_compiled_function_get_literal_frame_size(cfun));
  return (oop) literal_frame[idx];
}

number MMObj::mm_compiled_function_get_code_size(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[18];
}

bytecode* MMObj::mm_compiled_function_get_code(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (bytecode*) ((oop*)cfun)[19];
}

number MMObj::mm_compiled_function_exception_frames_count(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return (number) ((oop*)cfun)[20];
}

oop MMObj::mm_compiled_function_exception_frames(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[21];
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

  // number size = mm_dictionary_size(cfuns_dict);
  oop funs_dict = mm_dictionary_new();

  std::map<oop, oop>::iterator it = mm_dictionary_begin(cfuns_dict);
  for ( ; it != mm_dictionary_end(cfuns_dict); it++) {
    oop str_name = it->first;
    char* str = mm_string_cstr(str_name);
    oop cfun = it->second;
    oop fun = mm_function_from_cfunction(cfun, imod);
    mm_dictionary_set(funs_dict, _vm->new_symbol(str), fun);
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
  assert( *(oop*) klass != _core_image->get_prime("Behavior"));
  return ((oop*)klass)[4];
}


oop MMObj::mm_new_slot_getter(oop imodule, oop owner, oop name, int idx) {
  // assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  assert( *(oop*) name == _core_image->get_prime("String"));

  oop cfun_getter = (oop) calloc(sizeof(word), 17);

  * (oop*) cfun_getter = _core_image->get_prime("CompiledFunction");
  * (oop*) &cfun_getter[1] = mm_object_new();
  * (oop*) &cfun_getter[2] = name;
  * (oop*) &cfun_getter[3] = mm_list_new();
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
  for (unsigned long i = 0; i <= strlen(str); i++) {
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
  oop obj = (oop) calloc(sizeof(word), (2 + payload)); // vt, delegate

  ((oop*) obj)[0] = vt;
  ((oop*) obj)[1] = delegate;
  return obj;
}

oop MMObj::alloc_instance(oop klass) {
  if (klass == NULL) {
    // debug() << "alloc_instance: klass is null" << endl;
    return NULL;
  }

  // debug() << "alloc_instance for klass " << klass << endl;
  // debug() << "alloc_instance: class name: " << _mmobj->mm_string_cstr(_mmobj->mm_class_name(klass)) << endl;

  number payload = mm_behavior_size(klass);
  if (payload == INVALID_PAYLOAD) {
    bail("new_delegate: Received flagged/behavior payload");
  }

  debug() << "alloc_instance: payload " << payload << endl;

  oop instance = (oop) calloc(sizeof(word), payload + 2); //2: vt, delegate
  debug() << "new instance [size: " << payload << "]: "
          << mm_string_cstr(mm_class_name(klass)) << " = " << instance << endl;

  oop klass_parent =  mm_object_delegate(klass);

  ((oop*)instance)[0] = klass;
  ((oop*) instance)[1] = alloc_instance(klass_parent);

  debug() << "Created recursive delegate " << instance << endl;
  return instance;
}
