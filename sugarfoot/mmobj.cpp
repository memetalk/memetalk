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
