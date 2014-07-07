#include "mmobj.hpp"
#include "defs.hpp"
#include "core_loader.hpp"
#include <cstdlib>


oop mm_module_new(int num_classes, oop delegate) {
  oop imodule = (oop) malloc(sizeof(word) * (3 + num_classes)); //3: vt, delegate, dict

  ((word**) imodule)[0] = imodule; //imodule[vt] = imodule
  ((word**) imodule)[1] = delegate; // imodule[delegate]
  return imodule;
}

oop mm_compiled_module_classes(oop cmod) {
  return (oop) ((word*)cmod)[6];
}

oop mm_compiled_module_functions(oop cmod) {
  return (oop) ((word*)cmod)[5];
}

oop mm_object_new(CoreImage* core) {
  oop obj = (oop) malloc(sizeof(word) * 2); // vt, delegate

  ((word**) obj)[0] = core->get_prime("Object");
  ((word**) obj)[1] = 0;
  return obj;
}

oop mm_list_new_empty(CoreImage* core) {
  oop obj = (oop) malloc(sizeof(word) * 3); // vt, delegate, size

  ((word**) obj)[0] = core->get_prime("List");
  ((word**) obj)[1] = mm_object_new(core);
  ((word**) obj)[2] = 0;
  return obj;
}

oop mm_dictionary_new(int num_entries, CoreImage* core) {
  int basic = 3; //vt, delegate, size
  int payload = num_entries * 2;
  oop obj = (oop) malloc(sizeof(word) * (basic + payload));

  ((word**) obj)[0] = core->get_prime("Dictionary");
  ((word**) obj)[1] = mm_object_new(core);
  ((word*) obj)[2] = num_entries;
  return obj;
}

number mm_dictionary_size(oop dict) {
  return (number) ((word*)dict)[2];
}

void mm_module_set_dictionary(oop imodule, oop imod_dict) {
  ((word**) imodule)[2] = imod_dict;
}

oop mm_dictionary_entry_name(oop dict, int idx) {
  //0: vt
  //1: delegate
  //2: size
  return (word*) dict[3 + (idx * 2)];
}

oop mm_dictionary_entry_value(oop dict, int idx) {
  //0: vt
  //1: delegate
  //2: size
  return (word*) dict[3 + ((idx * 2)+1)];
}

void mm_dictionary_set(oop dict, int idx, oop key, oop value) {
  * (oop*) &dict[3 + (idx * 2)] = key;
  * (oop*) &dict[3 + ((idx * 2)+1)] = value;
}

char* mm_string_cstr(oop str) {
  //0: vt
  //1: delegate
  //2: size
  //3: <str> ...
  return (char*) &(str[3]);
}

oop mm_function_from_cfunction(oop cfun, oop imod, CoreImage* core) {
  oop fun = (oop) malloc(sizeof(word) * 4); //vt, delegate, cfun, module

  * (oop*) fun = core->get_prime("Function");
  * (oop*) &fun[1] = mm_object_new(core);
  * (oop*) &fun[2] = cfun;
  * (oop*) &fun[3] = imod;
  return fun;
}

oop mm_function_get_module(oop fun) {
  return (oop) ((word*)fun)[3];
}

oop mm_compiled_class_super_name(oop cclass) {
  return (oop) ((word*)cclass)[4];
}

oop mm_compiled_class_own_methods(oop cclass) {
  return (oop) ((word*)cclass)[7];
}

oop mm_cfuns_to_funs_dict(oop cfuns_dict, oop imod, CoreImage* core) {
  number size = mm_dictionary_size(cfuns_dict);
  oop funs_dict = mm_dictionary_new(size, core);
  for (int i = 0; i < size; i++) {
    oop str_name = mm_dictionary_entry_name(cfuns_dict, i);
    oop cfun = mm_dictionary_entry_value(cfuns_dict, i);
    oop fun = mm_function_from_cfunction(cfun, imod, core);
    mm_dictionary_set(funs_dict, i, str_name, fun);
  }
  return funs_dict;
}

oop mm_class_behavior_new(oop super_class, oop funs_dict, CoreImage* core) {
  //vt: Behavior
  //delegate:
  //dict
  oop cbehavior = (oop) malloc(sizeof(word) * 3);
  * (oop*) cbehavior = core->get_prime("Behavior");
  * (oop*) &cbehavior[1] = * (oop*) super_class; //super_class[vt]
  * (oop*) &cbehavior[2] = funs_dict;
  return cbehavior;
}


oop mm_class_new(oop class_behavior, oop super_class, oop dict, oop compiled_class) {
  oop klass = (oop) malloc(sizeof(word) * 4);
  * (oop*) klass = class_behavior;
  * (oop*) &klass[1] = super_class;
  * (oop*) &klass[2] = dict;
  * (oop*) &klass[3] = compiled_class;
  return klass;
}


oop mm_new_class_getter(oop imodule, oop cclass, oop name, int idx, CoreImage* core) {
  oop cfun_getter = (oop) calloc(sizeof(word), 12);

  * (oop*) cfun_getter = core->get_prime("CompiledFunction");
  * (oop*) &cfun_getter[1] = mm_object_new(core);
  * (oop*) &cfun_getter[2] = name;
  * (oop*) &cfun_getter[3] = mm_list_new_empty(core);
  * (word*) &cfun_getter[7] = 1;
  * (word*) &cfun_getter[8] = idx;
  * (oop*) &cfun_getter[9] = cclass;

  return mm_function_from_cfunction(cfun_getter, imodule, core);
}

oop mm_symbol_new(const char* str, CoreImage* core) {
  oop symb = (oop) malloc((sizeof(word) * 3) + (strlen(str)+1));

  * (oop*) symb = core->get_prime("Symbol");
  * (oop*) &symb[1] = mm_object_new(core);
  * (word*) &symb[2] = strlen(str);

  char* target = (char*) &symb[3];
  for (int i = 0; i <= strlen(str); i++) {
    target[i] = str[i];
  }
  return symb;
}
