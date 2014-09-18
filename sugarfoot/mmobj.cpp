#include "mmobj.hpp"
#include "defs.hpp"
#include "core_image.hpp"
#include <cstdlib>
#include "report.hpp"
#include "vm.hpp"
#include <assert.h>
#include "utils.hpp"

#include <map>

MMObj::MMObj(CoreImage* core)
  : _core_image(core) {
}

oop MMObj::mm_process_new(Process* proc) {
  oop obj = alloc_instance(_core_image->get_prime("Process"));
  ((oop*)obj)[2] = (oop) proc;
  return obj;
}

Process* MMObj::mm_process_get_proc(oop proc) {
  assert( mm_object_vt(proc) == _core_image->get_prime("Process"));
  return (Process*) (((oop*)proc)[2]);
}

oop MMObj::mm_frame_new(oop bp) {
  oop obj = alloc_instance(_core_image->get_prime("Frame"));
  ((oop*)obj)[2] = bp;
  return obj;
}

oop MMObj::mm_frame_get_bp(oop frame) {
  assert( mm_object_vt(frame) == _core_image->get_prime("Frame"));
  return ((oop*)frame)[2];
}

oop MMObj::mm_frame_get_cp(oop frame) {
  assert( mm_object_vt(frame) == _core_image->get_prime("Frame"));
  oop bp = mm_frame_get_bp(frame);
  return *((oop*)bp - 3);
}

oop MMObj::mm_frame_get_fp(oop frame) {
  assert( mm_object_vt(frame) == _core_image->get_prime("Frame"));
  oop bp = mm_frame_get_bp(frame);
  return *((oop*)bp - 4);
}

bytecode* MMObj::mm_frame_get_ip(oop frame) {
  assert( mm_object_vt(frame) == _core_image->get_prime("Frame"));
  oop bp = mm_frame_get_bp(frame);
  return (bytecode*) *((oop*)bp - 2);
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
  assert( mm_object_vt(cmod) == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[2];
}

//license 3

oop MMObj::mm_compiled_module_params(oop cmod) {
  assert( mm_object_vt(cmod) == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[4];
}


oop MMObj::mm_compiled_module_default_params(oop cmod) {
  assert( mm_object_vt(cmod) == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[5];
}

oop MMObj::mm_compiled_module_aliases(oop cmod) {
  assert( mm_object_vt(cmod) == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[6];
}

oop MMObj::mm_compiled_module_functions(oop cmod) {
  assert( mm_object_vt(cmod) == _core_image->get_prime("CompiledModule"));
  return (oop) ((word*)cmod)[7];
}

oop MMObj::mm_compiled_module_classes(oop cmod) {
  assert( mm_object_vt(cmod) == _core_image->get_prime("CompiledModule"));
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
  assert( mm_object_vt(list) == _core_image->get_prime("List"));
  std::vector<oop>* elements = mm_list_frame(list);
  return elements->size();
}

oop MMObj::mm_list_entry(oop list, number idx) {
  assert( mm_object_vt(list) == _core_image->get_prime("List"));
  std::vector<oop>* elements = mm_list_frame(list);
  return (*elements)[idx];
}

std::vector<oop>* MMObj::mm_list_frame(oop list) {
  assert( mm_object_vt(list) == _core_image->get_prime("List"));
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
  assert( mm_object_vt(list) == _core_image->get_prime("List"));
  std::vector<oop>* elements = mm_list_frame(list);


  std::vector<oop>::iterator it = elements->begin();
  for (number pos = 0; it != elements->end(); it++, pos++) {
    if (*it == elem) {
      return pos;
    //some temporary hack while we don't have a self-host compiler doing Object.==
    } else if (mm_is_string(elem) && mm_is_string(*it)) {
      debug() << mm_string_cstr(elem) << " <cmp> " << mm_string_cstr(*it) << endl;
      if (strcmp(mm_string_cstr(elem), mm_string_cstr(*it)) == 0) {
        return pos;
      }
    }
  }
  return -1;
}

void MMObj::mm_list_set(oop list, number idx, oop element) {
  assert( mm_object_vt(list) == _core_image->get_prime("List"));
  std::vector<oop>* elements = mm_list_frame(list);
  (*elements)[idx] = element;
}


void MMObj::mm_list_prepend(oop list, oop element) {
  assert( mm_object_vt(list) == _core_image->get_prime("List"));

  std::vector<oop>* elements = mm_list_frame(list);
  elements->insert(elements->begin(), element);
}

void MMObj::mm_list_append(oop list, oop element) {
  assert( mm_object_vt(list) == _core_image->get_prime("List"));

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
  assert( mm_object_vt(dict) == _core_image->get_prime("Dictionary"));

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
  assert( mm_object_vt(dict) == _core_image->get_prime("Dictionary"));
  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  return elements->begin();
}

std::map<oop,oop>::iterator MMObj::mm_dictionary_end(oop dict) {
  assert( mm_object_vt(dict) == _core_image->get_prime("Dictionary"));
  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  return elements->end();
}

number MMObj::mm_dictionary_size(oop dict) {
  assert( mm_object_vt(dict) == _core_image->get_prime("Dictionary"));
  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  return elements->size();
}

bool MMObj::mm_dictionary_has_key(oop dict, oop key) {
  assert( mm_object_vt(dict) == _core_image->get_prime("Dictionary"));

  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  // debug() << dict << "(" << elements->size() << ") has key " << key << " ?" << endl;
  for (std::map<oop, oop>::iterator it = elements->begin(); it != elements->end(); it++) {
    if (it->first == key) {
      return true;
    } else if (mm_is_string(key) && mm_is_string(it->first)) {
      // debug() << dict << "(" << elements->size() << ") has key " << mm_string_cstr(key) << " ?=" << mm_string_cstr(it->first) << endl;
      if (strcmp(mm_string_cstr(key), mm_string_cstr(it->first)) == 0) {
        return true;
      }
    }
  }
  return false;
}

oop MMObj::mm_dictionary_keys(oop dict) {
  assert( mm_object_vt(dict) == _core_image->get_prime("Dictionary"));

  oop lst = mm_list_new();
  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  for (std::map<oop, oop>::iterator it = elements->begin(); it != elements->end(); it++) {
    mm_list_append(lst, it->first);
  }
  return lst;
}

oop MMObj::mm_dictionary_get(oop dict, oop key) {
  assert( mm_object_vt(dict) == _core_image->get_prime("Dictionary"));

  std::map<oop, oop>* elements = mm_dictionary_frame(dict);

  debug() << dict << "(" << elements->size() << ") get " << key << endl;
  for (std::map<oop, oop>::iterator it = elements->begin(); it != elements->end(); it++) {
    // debug() << dict << "(" << elements->size() << ") get direct? " << (it->first == key) << endl;
    if (it->first == key) {
      return it->second;
    } else {
      // debug() << dict << "(" << elements->size() << ") get string? " << (mm_is_string(key) && mm_is_string(it->first)) << endl;
      if (mm_is_string(key) && mm_is_string(it->first)) {
        // debug() << dict << "(" << elements->size() << ") get: " << mm_is_string(key) << " =?= " << mm_string_cstr(it->first) << endl;
        if (strcmp(mm_string_cstr(key), mm_string_cstr(it->first)) == 0) {
          return it->second;
        }
      }
    }
  }
  return MM_NULL;
}


void MMObj::mm_dictionary_set(oop dict, oop key, oop value) {
  assert( mm_object_vt(dict) == _core_image->get_prime("Dictionary"));

  std::map<oop, oop>* elements = mm_dictionary_frame(dict);
  (*elements)[key] = value;
}


void MMObj::mm_module_set_dictionary(oop imodule, oop imod_dict) {
  assert( mm_object_vt(imod_dict) == _core_image->get_prime("Dictionary"));
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
  assert( mm_object_vt(str) == _core_image->get_prime("String"));
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
  assert( mm_object_vt(ctx) == _core_image->get_prime("Context"));
  return ((oop*)ctx)[4];
}


bool MMObj::mm_function_uses_env(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_uses_env(cfun);
}

oop MMObj::mm_function_from_cfunction(oop cfun, oop imod) {
  assert( mm_object_vt(cfun) == _core_image->get_prime("CompiledFunction"));

  oop fun = (oop) malloc(sizeof(word) * 4); //vt, delegate, cfun, module

  * (oop*) fun = _core_image->get_prime("Function");
  * (oop*) &fun[1] = mm_object_new();
  * (oop*) &fun[2] = cfun;
  * (oop*) &fun[3] = imod;
  return fun;
}

oop MMObj::mm_function_get_module(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  return (oop) ((oop*)fun)[3];
}

oop MMObj::mm_function_get_cfun(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  return (oop) ((oop*)fun)[2];
}

bool MMObj::mm_function_is_prim(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_is_prim(cfun);
}

oop MMObj::mm_function_get_name(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_name(cfun);
}

oop MMObj::mm_function_get_prim_name(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_prim_name(cfun);
}


bytecode* MMObj::mm_function_get_code(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_code(cfun);
}

number MMObj::mm_function_get_code_size(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_code_size(cfun);
}

oop MMObj::mm_function_get_literal_by_index(oop fun, int idx) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_literal_by_index(cfun, idx);
}

number MMObj::mm_function_get_num_params(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_num_params(cfun);
}

number MMObj::mm_function_get_num_locals_or_env(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_num_locals_or_env(cfun);
}

number MMObj::mm_function_get_env_offset(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_env_offset(cfun);
}

bool MMObj::mm_function_is_getter(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_is_getter(cfun);
}

number MMObj::mm_function_access_field(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_access_field(cfun);
}

oop MMObj::mm_function_get_owner(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_owner(cfun);
}

bool MMObj::mm_function_is_ctor(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_is_ctor(cfun);
}

number MMObj::mm_function_exception_frames_count(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_exception_frames_count(cfun);
}

oop MMObj::mm_function_exception_frames(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_exception_frames(cfun);
}

oop MMObj::mm_function_env_table(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_env_table(cfun);
}

oop MMObj::mm_function_get_text(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_text(cfun);
}

oop MMObj::mm_function_get_line_mapping(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_line_mapping(cfun);
}

oop MMObj::mm_function_get_loc_mapping(oop fun) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_get_loc_mapping(cfun);
}

bytecode* MMObj::mm_function_next_expr(oop fun, bytecode* ip) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_next_expr(cfun, ip);
}

bytecode* MMObj::mm_function_next_line_expr(oop fun, bytecode* ip) {
  assert( mm_object_vt(fun) == _core_image->get_prime("Function") ||
          mm_object_vt(fun) == _core_image->get_prime("Context"));
  oop cfun = mm_function_get_cfun(fun);
  return mm_compiled_function_next_line_expr(cfun, ip);
}


bool MMObj::mm_compiled_function_is_ctor(oop cfun) {
  assert( mm_object_vt(cfun) == _core_image->get_prime("CompiledFunction"));
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

oop MMObj::mm_compiled_function_env_table(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[22];
}

oop MMObj::mm_compiled_function_get_text(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[23];
}

oop MMObj::mm_compiled_function_get_line_mapping(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[24];
}

oop MMObj::mm_compiled_function_get_loc_mapping(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[25];
}

oop MMObj::mm_compiled_function_get_cmod(oop cfun) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  return ((oop*)cfun)[26];
}

void MMObj::mm_compiled_function_set_cmod(oop cfun, oop cmod) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));
  ((oop*)cfun)[27] = cmod;
}

bytecode* MMObj::mm_compiled_function_next_expr(oop cfun, bytecode* ip) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));

  bytecode* base_ip = mm_compiled_function_get_code(cfun);
  word idx = ip - base_ip;

  oop mapping = mm_compiled_function_get_loc_mapping(cfun);
  std::map<oop, oop>::iterator it = mm_dictionary_begin(mapping);
  std::map<oop, oop>::iterator end = mm_dictionary_end(mapping);
  word next_offset = INT_MAX;
  for ( ; it != end; it++) {
    word b_offset = untag_small_int(it->first);
    if ((idx < b_offset) && (next_offset > b_offset)) {
      next_offset = b_offset;
    }
  }
  if (next_offset == INT_MAX) {
    std::cerr << "WARNING: next_expr for " << idx << " is NULL" << endl;
    return NULL;
  } else {
    return base_ip + next_offset;
  }
}

bytecode* MMObj::mm_compiled_function_next_line_expr(oop cfun, bytecode* ip) {
  assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));

  bytecode* base_ip = mm_compiled_function_get_code(cfun);
  word idx = ip - base_ip;

  oop mapping = mm_compiled_function_get_line_mapping(cfun);
  std::map<oop, oop>::iterator it = mm_dictionary_begin(mapping);
  std::map<oop, oop>::iterator end = mm_dictionary_end(mapping);
  word current_line = 0;
  for ( ; it != end; it++) {
    word b_offset = untag_small_int(it->first);
    word line = untag_small_int(it->second);
    // std::cerr << " SEARCH CURR LINE "
    //           << idx << " " << b_offset << " " << line << endl;
    if ((idx >= b_offset) && (current_line < line)) {
      // std::cerr << "GOT LINE " << line << endl;
      current_line = line;
    }
  }

  it = mm_dictionary_begin(mapping);
  word next_line = INT_MAX;
  word next_offset = 0;
  for ( ; it != end; it++) {
    word line = untag_small_int(it->second);
    // std::cerr << "line: " << line << " " << current_line << " " << next_line << endl;
    if (line > current_line && next_line > line) {
      next_line = line;
      next_offset = untag_small_int(it->first);
    }
  }

  if (next_line == INT_MAX) {
    std::cerr << "WARNING: next_line for " << idx << " is NULL" << endl;
    return NULL;
  } else {
    // std::cerr << " CURR LINE " << current_line << " NEXT: " << next_line
    //           << " offset: " << next_offset << endl;
    return base_ip + next_offset;
  }
}

// bool MMObj::mm_compiled_function_loc_mapping_matches_ip(oop cfun, bytecode* ip) {
//   assert( *(oop*) cfun == _core_image->get_prime("CompiledFunction"));

//   bytecode* base_ip = mm_compiled_function_get_code(cfun);
//   word idx = ip - base_ip;
//   // std::cerr << "MMOBJ IDX : " << idx << endl;

//   oop mapping = mm_compiled_function_get_loc_mapping(cfun);
//   std::map<oop, oop>::iterator it = mm_dictionary_begin(mapping);
//   std::map<oop, oop>::iterator end = mm_dictionary_end(mapping);
//   for ( ; it != end; it++) {
//     word b_offset = untag_small_int(it->first);
//     // std::cerr << "LOC_MATCHES_IP? -- " << b_offset << " " <<  idx << std::endl;
//     if (idx == b_offset) {
//       return true;
//     }
//     if (b_offset > idx) {
//       break;
//     }
//   }
//   // std::cerr << "LOC_MATCHES_IP? RET FALSE: " << idx << std::endl;
//   return false;
// }



oop MMObj::mm_compiled_class_name(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return ((oop*)cclass)[2];
}

oop MMObj::mm_compiled_class_super_name(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return ((oop*)cclass)[3];
}

oop MMObj::mm_compiled_class_fields(oop cclass) {
  assert( *(oop*) cclass == _core_image->get_prime("CompiledClass"));
  return ((oop*)cclass)[4];
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
    oop sym_name = it->first;
    oop cfun = it->second;
    oop fun = mm_function_from_cfunction(cfun, imod);
    mm_dictionary_set(funs_dict, sym_name, fun);
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

bool MMObj::mm_is_symbol(oop sym) {
  return *(oop*) sym == _core_image->get_prime("Symbol");
}

char* MMObj::mm_symbol_cstr(oop sym) {
  assert( *(oop*) sym == _core_image->get_prime("Symbol"));
  //0: vt
  //1: delegate
  //2: size
  //3: <str> ...
  return (char*) &(sym[3]);
}

oop MMObj::mm_symbol_to_string(oop sym) {
  assert( *(oop*) sym == _core_image->get_prime("Symbol"));
  return mm_string_new(mm_symbol_cstr(sym));
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

bool MMObj::delegates_to(oop sub_type, oop super_type) {
  debug() << "delegates_to? " << sub_type << " " << super_type << endl;
  if (sub_type == NULL) {
    return false;
  }
  if (sub_type == super_type) {
    return true;
  } else {
    return delegates_to(mm_object_delegate(sub_type), super_type);
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
    debug() << "alloc_instance: klass is null" << endl;
    return NULL;
  }

  debug() << "alloc_instance for klass " << klass << endl;
  debug() << "alloc_instance: class name: " << mm_string_cstr(mm_class_name(klass)) << endl;

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


bool MMObj::mm_is_list(oop obj) {
  return *(oop*) obj == _core_image->get_prime("List");
}

bool MMObj::mm_is_dictionary(oop obj) {
  return *(oop*) obj == _core_image->get_prime("Dictionary");
}


std::list<std::string> MMObj::mm_sym_list_to_cstring_list(oop lst) {
  assert( *(oop*) lst == _core_image->get_prime("List"));

  std::list<std::string> res;
  for (int i = 0; i < mm_list_size(lst); i++) {
    res.push_back(mm_symbol_cstr(mm_list_entry(lst, i)));
  }
  return res;
}
