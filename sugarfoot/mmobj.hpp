#ifndef MMOBJ_HPP
#define MMOBJ_HPP

#include "defs.hpp"

class CoreImage;
class VM;

class MMObj {
public:
  MMObj(VM*, CoreImage*);

  oop mm_module_new(int num_classes, oop delegate);

  oop mm_compiled_module_classes(oop);
  oop mm_compiled_module_functions(oop);

  oop mm_object_new();
  oop mm_object_vt(oop);
  oop mm_object_delegate(oop);

  oop mm_list_new_empty();
  number mm_list_size(oop list);

  oop mm_dictionary_new(int num_entries);
  number mm_dictionary_size(oop dict);
  oop mm_dictionary_entry_key(oop dict, int idx);
  oop mm_dictionary_entry_value(oop dict, int idx);
  void mm_dictionary_set(oop dict, int idx, oop key, oop value);
  bool mm_dictionary_has_key(oop dict, oop key);
  oop mm_dictionary_get(oop dict, oop key);


  void mm_module_set_dictionary(oop imodule, oop imod_dict);

  bool mm_is_string(oop);
  char* mm_string_cstr(oop);

  oop mm_symbol_new(const char* str);
  char* mm_symbol_cstr(oop);

  oop mm_function_from_cfunction(oop cfun, oop imod);
  bool mm_function_is_prim(oop fun);

  oop mm_function_get_module(oop fun);
  oop mm_function_get_prim_name(oop fun);
  oop mm_function_get_cfun(oop fun);
  bytecode* mm_function_get_code(oop fun);
  number mm_function_get_code_size(oop fun);
  oop mm_function_get_literal_by_index(oop fun, int idx);
  number mm_function_get_num_locals(oop fun);
  number mm_function_get_num_params(oop fun);
  bool mm_function_is_ctor(oop fun);
  bool mm_function_is_getter(oop fun);
  number mm_function_access_field(oop fun);

  bytecode* mm_compiled_function_get_code(oop cfun);
  number mm_compiled_function_get_code_size(oop cfun);
  number mm_compiled_function_get_num_locals(oop cfun);
  number mm_compiled_function_get_num_params(oop cfun);
  bool mm_compiled_function_is_getter(oop cfun);
  number mm_compiled_function_access_field(oop cfun);
  bool mm_compiled_function_is_ctor(oop cfun);
  bool mm_compiled_function_is_prim(oop cfun);
  oop mm_compiled_function_get_prim_name(oop cfun);

  oop mm_compiled_class_name(oop cclass);
  oop mm_compiled_class_super_name(oop cclass);
  oop mm_compiled_class_own_methods(oop cclass);
  oop mm_compiled_class_methods(oop cclass);
  number mm_compiled_class_num_fields(oop cclass);

  oop mm_compiled_function_get_literal_by_index(oop cfun, int idx);
  number mm_compiled_function_get_literal_frame_size(oop cfun);

  oop mm_class_behavior_new(oop super_class, oop funs_dict);
  oop mm_class_new(oop class_behavior, oop super_class, oop dict, oop compiled_class, number payload);
  oop mm_class_name(oop klass);
  oop mm_class_get_compiled_class(oop klass);
  oop mm_class_dict(oop);

  oop mm_cfuns_to_funs_dict(oop cfuns_dict, oop imod);
  oop mm_new_class_getter(oop imodule, oop cclass, oop name, int idx);

  oop mm_behavior_get_dict(oop);
  number mm_behavior_size(oop);

private:
  VM* _vm;
  CoreImage* _core_image;
};

#endif
