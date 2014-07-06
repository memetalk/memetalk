#ifndef MMOBJ_HPP
#define MMOBJ_HPP

#include "defs.hpp"

class CoreImage;

oop mm_module_new(int num_classes, oop delegate);

oop mm_compiled_module_classes(oop);
oop mm_compiled_module_functions(oop);

oop mm_object_new(CoreImage* core);

oop mm_dictionary_new(int num_entries, CoreImage* core);
number mm_dictionary_size(oop dict);

void mm_module_set_dictionary(oop imodule, oop imod_dict);

oop mm_dictionary_entry_name(oop dict, int idx);
oop mm_dictionary_entry_value(oop dict, int idx);
void mm_dictionary_set(oop dict, int idx, oop key, oop value);

char* mm_string_cstr(oop);


oop mm_function_from_cfunction(oop cfun, oop imod, CoreImage* core);

#endif
