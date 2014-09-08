#ifndef MMOBJ_HPP
#define MMOBJ_HPP

#include "defs.hpp"
#include <vector>
#include <map>

class CoreImage;
class VM;

class MMObj {
public:
  MMObj(CoreImage*);

  oop mm_process_new(Process* proc);

  oop mm_new(oop vt, oop delegate, number payload);
  oop alloc_instance(oop klass);

  oop mm_object_new();
  oop mm_object_vt(oop);
  oop mm_object_delegate(oop);

  oop mm_behavior_get_dict(oop);
  number mm_behavior_size(oop);

  bool delegates_to(oop, oop);

  oop mm_module_new(int num_fields, oop cmod, oop delegate);

  oop mm_compiled_module_classes(oop);
  oop mm_compiled_module_name(oop);
  oop mm_compiled_module_functions(oop);
  oop mm_compiled_module_params(oop);
  oop mm_compiled_module_default_params(oop);
  oop mm_compiled_module_aliases(oop);

  oop mm_boolean_new(number val);
  bool mm_boolean_cbool(oop val);

  oop mm_list_new();
  number mm_list_size(oop list);
  number mm_list_index_of(oop list, oop elem);
  oop mm_list_entry(oop list, number idx);
  std::vector<oop>* mm_list_frame(oop);
  void mm_list_prepend(oop list, oop element);
  void mm_list_append(oop list, oop element);
  void mm_list_set(oop list, number idx, oop element);

  oop mm_dictionary_new();
  number mm_dictionary_size(oop dict);
  // oop mm_dictionary_entry_key(oop dict, int idx);
  // oop mm_dictionary_entry_value(oop dict, int idx);
  void mm_dictionary_set(oop dict, oop key, oop value);
  bool mm_dictionary_has_key(oop dict, oop key);
  oop mm_dictionary_get(oop dict, oop key);
  std::map<oop,oop>* mm_dictionary_frame(oop);
  std::map<oop,oop>::iterator mm_dictionary_begin(oop);
  std::map<oop,oop>::iterator mm_dictionary_end(oop);

  void mm_module_set_dictionary(oop imodule, oop imod_dict);
  void mm_module_set_module_argument(oop imodule, oop arg, number idx);
  oop mm_module_entry(oop imodule, number idx);
  oop mm_module_dictionary(oop imodule);
  oop mm_module_get_cmod(oop imodule);
  oop mm_module_get_param(oop imodule, number idx);

  bool mm_is_string(oop);
  oop mm_string_new(const char*);
  char* mm_string_cstr(oop);


  bool mm_is_list(oop);
  bool mm_is_dictionary(oop);

  oop mm_symbol_new(const char* str);
  bool mm_is_symbol(oop);
  char* mm_symbol_cstr(oop);
  oop mm_symbol_to_string(oop);

  bool mm_is_function(oop);
  bool mm_is_context(oop);

  oop mm_context_get_env(oop);

  oop mm_function_from_cfunction(oop cfun, oop imod);
  bool mm_function_is_prim(oop fun);
  oop mm_function_get_name(oop fun);

  oop mm_function_get_module(oop fun);
  oop mm_function_get_prim_name(oop fun);
  oop mm_function_get_cfun(oop fun);
  bytecode* mm_function_get_code(oop fun);
  number mm_function_get_code_size(oop fun);
  oop mm_function_get_literal_by_index(oop fun, int idx);
  number mm_function_get_num_locals_or_env(oop fun);
  number mm_function_get_env_offset(oop fun);
  number mm_function_get_num_params(oop fun);
  bool mm_function_is_ctor(oop fun);
  bool mm_function_is_getter(oop fun);
  bool mm_function_uses_env(oop fun);
  number mm_function_access_field(oop fun);

  oop mm_function_get_owner(oop fun);

  number mm_function_exception_frames_count(oop fun);
  oop mm_function_exception_frames(oop fun);
  oop mm_function_env_table(oop fun);

  oop mm_function_get_text(oop cfun);
  oop mm_function_get_line_mapping(oop cfun);
  oop mm_function_get_loc_mapping(oop cfun);

  bytecode* mm_compiled_function_get_code(oop cfun);
  number mm_compiled_function_get_code_size(oop cfun);
  number mm_compiled_function_get_num_locals_or_env(oop cfun);
  number mm_compiled_function_get_env_offset(oop cfun);
  number mm_compiled_function_get_num_params(oop cfun);
  bool mm_compiled_function_is_getter(oop cfun);
  number mm_compiled_function_access_field(oop cfun);
  bool mm_compiled_function_is_ctor(oop cfun);
  bool mm_compiled_function_is_prim(oop cfun);
  oop mm_compiled_function_get_prim_name(oop cfun);
  bool mm_compiled_function_uses_env(oop cfun);
  bool mm_compiled_function_is_top_level(oop cfun);
  oop mm_compiled_function_outer_cfun(oop cfun);
  oop mm_compiled_function_get_owner(oop cfun);
  oop mm_compiled_function_get_name(oop cfun);

  oop mm_compiled_function_get_text(oop cfun);
  oop mm_compiled_function_get_line_mapping(oop cfun);
  oop mm_compiled_function_get_loc_mapping(oop cfun);

  oop mm_compiled_class_name(oop cclass);
  oop mm_compiled_class_super_name(oop cclass);
  oop mm_compiled_class_own_methods(oop cclass);
  oop mm_compiled_class_methods(oop cclass);
  oop mm_compiled_class_fields(oop cclass);
  number mm_compiled_class_num_fields(oop cclass);

  oop mm_compiled_function_get_literal_by_index(oop cfun, int idx);
  number mm_compiled_function_get_literal_frame_size(oop cfun);

  number mm_compiled_function_exception_frames_count(oop cfun);
  oop mm_compiled_function_exception_frames(oop cfun);
  oop mm_compiled_function_env_table(oop cfun);

  oop mm_class_behavior_new(oop super_class, oop funs_dict);
  oop mm_class_new(oop class_behavior, oop super_class, oop dict, oop compiled_class, number payload);
  oop mm_class_name(oop klass);
  oop mm_class_get_compiled_class(oop klass);
  oop mm_class_dict(oop);

  oop mm_cfuns_to_funs_dict(oop cfuns_dict, oop imod);
  oop mm_new_slot_getter(oop imodule, oop owner, oop name, int idx);

  CoreImage* core() { return _core_image; };

private:
  // VM* _vm;
  CoreImage* _core_image;
};

#endif
