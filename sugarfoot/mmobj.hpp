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
oop mm_dictionary_entry_key(oop dict, int idx);
oop mm_dictionary_entry_value(oop dict, int idx);
void mm_dictionary_set(oop dict, int idx, oop key, oop value);
bool mm_dictionary_has_key(oop dict, oop key);
oop mm_dictionary_get(oop dict, oop key);


void mm_module_set_dictionary(oop imodule, oop imod_dict);


char* mm_string_cstr(oop);

oop mm_symbol_new(const char* str, CoreImage* core);

oop mm_function_from_cfunction(oop cfun, oop imod, CoreImage* core);
oop mm_function_get_module(oop fun);
bool mm_function_is_prim(oop fun);
oop mm_function_get_prim_name(oop fun);

oop mm_compiled_class_super_name(oop cclass);
oop mm_compiled_class_own_methods(oop cclass);

oop mm_class_behavior_new(oop super_class, oop funs_dict, CoreImage* core);
oop mm_class_new(oop class_behavior, oop super_class, oop dict, oop compiled_class);

oop mm_cfuns_to_funs_dict(oop cfuns_dict, oop imod, CoreImage* core);
oop mm_new_class_getter(oop imodule, oop cclass, oop name, int idx, CoreImage* core);

oop mm_behavior_get_dict(oop);

#endif
