#ifndef MMOBJ_HPP
#define MMOBJ_HPP

#include "defs.hpp"
#include <vector>
#include <map>
#include <list>

class CoreImage;
class VM;

class MMObj {
public:
  MMObj(CoreImage*);

  oop mm_process_new(Process*, Process* proc);
  Process* mm_process_get_proc(Process*, oop);
  oop mm_frame_new(Process*, oop);
  oop mm_frame_get_bp(Process*, oop);
  oop mm_frame_get_cp(Process*, oop);
  oop mm_frame_get_fp(Process*, oop);
  oop mm_frame_get_rp(Process*, oop);
  oop mm_frame_get_dp(Process*, oop);
  number mm_frame_get_ss(Process*, oop);
  bytecode* mm_frame_get_ip(Process*, oop);

  oop mm_new(oop vt, oop delegate, number payload);
  oop alloc_instance(Process*, oop klass);

  oop mm_object_new();
  oop mm_object_vt(oop);
  oop mm_object_delegate(oop);

  oop mm_behavior_get_dict(oop);
  number mm_behavior_size(Process*, oop);

  bool delegates_to(oop, oop);

  oop mm_module_new(int num_fields, oop cmod, oop delegate);

  oop mm_compiled_module_classes(Process*, oop);
  oop mm_compiled_module_name(Process*, oop);
  oop mm_compiled_module_functions(Process*, oop);
  oop mm_compiled_module_params(Process*, oop);
  oop mm_compiled_module_default_params(Process*, oop);
  oop mm_compiled_module_aliases(Process*, oop);

  oop mm_boolean_new(number val);
  bool mm_boolean_cbool(Process*, oop val);

  oop mm_list_new();
  number mm_list_size(Process*, oop list);
  number mm_list_index_of(Process*, oop list, oop elem);
  oop mm_list_entry(Process*, oop list, number idx);
  std::vector<oop>* mm_list_frame(Process*, oop);
  void mm_list_prepend(Process*, oop list, oop element);
  void mm_list_append(Process*, oop list, oop element);
  void mm_list_set(Process*, oop list, number idx, oop element);

  oop mm_dictionary_new();
  number mm_dictionary_size(Process*, oop dict);
  // oop mm_dictionary_entry_key(Process*, oop dict, int idx);
  // oop mm_dictionary_entry_value(Process*, oop dict, int idx);
  void mm_dictionary_set(Process*, oop dict, oop key, oop value);
  bool mm_dictionary_has_key(Process*, oop dict, oop key);
  oop mm_dictionary_keys(Process*, oop dict);
  oop mm_dictionary_get(Process*, oop dict, oop key);
  std::map<oop,oop>* mm_dictionary_frame(Process*, oop);
  std::map<oop,oop>::iterator mm_dictionary_begin(Process*, oop);
  std::map<oop,oop>::iterator mm_dictionary_end(Process*, oop);

  void mm_module_set_dictionary(Process*, oop imodule, oop imod_dict);
  void mm_module_set_module_argument(oop imodule, oop arg, number idx);
  oop mm_module_entry(oop imodule, number idx);
  oop mm_module_dictionary(oop imodule);
  oop mm_module_get_cmod(oop imodule);
  oop mm_module_get_param(oop imodule, number idx);

  bool mm_is_string(oop);
  oop mm_string_new(const char*);
  char* mm_string_cstr(Process*, oop);


  bool mm_is_list(oop);
  bool mm_is_dictionary(oop);

  oop mm_symbol_new(const char* str);
  bool mm_is_symbol(oop);
  char* mm_symbol_cstr(Process*, oop);
  oop mm_symbol_to_string(Process*, oop);

  bool mm_is_function(oop);
  bool mm_is_context(oop);

  void mm_context_set_cfun(Process*, oop, oop);
  void mm_context_set_env(Process*, oop, oop);
  void mm_context_set_module(Process*, oop, oop);
  oop mm_context_get_env(Process*, oop);

  oop mm_function_from_cfunction(Process*, oop cfun, oop imod);
  bool mm_function_is_prim(Process*, oop fun);
  oop mm_function_get_name(Process*, oop fun);

  oop mm_function_get_module(Process*, oop fun);
  oop mm_function_get_prim_name(Process*, oop fun);
  oop mm_function_get_cfun(Process*, oop fun);
  bytecode* mm_function_get_code(Process*, oop fun);
  number mm_function_get_code_size(Process*, oop fun);
  oop mm_function_get_literal_by_index(Process*, oop fun, int idx);
  number mm_function_get_num_locals_or_env(Process*, oop fun);
  number mm_function_get_env_offset(Process*, oop fun);
  number mm_function_get_num_params(Process*, oop fun);
  bool mm_function_is_ctor(Process*, oop fun);
  bool mm_function_is_getter(Process*, oop fun);
  bool mm_function_uses_env(Process*, oop fun);
  number mm_function_access_field(Process*, oop fun);

  oop mm_function_get_owner(Process*, oop fun);

  number mm_function_exception_frames_count(Process*, oop fun);
  oop mm_function_exception_frames(Process*, oop fun);
  oop mm_function_env_table(Process*, oop fun);

  oop mm_function_get_text(Process*, oop fun);
  oop mm_function_get_line_mapping(Process*, oop fun);
  oop mm_function_get_loc_mapping(Process*, oop fun);
  bytecode* mm_function_next_expr(Process*, oop fun, bytecode* ip);
  bytecode* mm_function_next_line_expr(Process*, oop fun, bytecode* ip);

  bytecode* mm_compiled_function_get_code(Process*, oop cfun);
  number mm_compiled_function_get_code_size(Process*, oop cfun);
  number mm_compiled_function_get_num_locals_or_env(Process*, oop cfun);
  number mm_compiled_function_get_env_offset(Process*, oop cfun);
  number mm_compiled_function_get_num_params(Process*, oop cfun);
  bool mm_compiled_function_is_getter(Process*, oop cfun);
  number mm_compiled_function_access_field(Process*, oop cfun);
  bool mm_compiled_function_is_ctor(Process*, oop cfun);
  bool mm_compiled_function_is_prim(Process*, oop cfun);
  oop mm_compiled_function_get_prim_name(Process*, oop cfun);
  bool mm_compiled_function_uses_env(Process*, oop cfun);
  bool mm_compiled_function_is_top_level(Process*, oop cfun);
  oop mm_compiled_function_outer_cfun(Process*, oop cfun);
  oop mm_compiled_function_get_owner(Process*, oop cfun);
  void mm_compiled_function_set_owner(Process*, oop cfun, oop owner);
  oop mm_compiled_function_get_name(Process*, oop cfun);

  oop mm_compiled_function_get_text(Process*, oop cfun);
  oop mm_compiled_function_get_line_mapping(Process*, oop cfun);
  oop mm_compiled_function_get_loc_mapping(Process*, oop cfun);
  // oop mm_compiled_function_get_cmod(Process*, oop cfun);
  void mm_compiled_function_set_cmod(Process*, oop cfun, oop cmod);

  bool mm_compiled_function_loc_mapping_matches_ip(Process*, oop, bytecode*);
  bytecode* mm_compiled_function_next_expr(Process*, oop cfun, bytecode* ip);
  bytecode* mm_compiled_function_next_line_expr(Process*, oop cfun, bytecode* ip);

  oop mm_compiled_class_name(Process*, oop cclass);
  oop mm_compiled_class_super_name(Process*, oop cclass);
  oop mm_compiled_class_own_methods(Process*, oop cclass);
  oop mm_compiled_class_methods(Process*, oop cclass);
  oop mm_compiled_class_fields(Process*, oop cclass);
  number mm_compiled_class_num_fields(Process*, oop cclass);

  oop mm_compiled_function_get_literal_by_index(Process*, oop cfun, int idx);
  number mm_compiled_function_get_literal_frame_size(Process*, oop cfun);

  number mm_compiled_function_exception_frames_count(Process*, oop cfun);
  oop mm_compiled_function_exception_frames(Process*, oop cfun);
  oop mm_compiled_function_env_table(Process*, oop cfun);

  oop mm_class_behavior_new(Process*, oop super_class, oop funs_dict);
  oop mm_class_new(Process*, oop class_behavior, oop super_class, oop dict, oop compiled_class, number payload);
  oop mm_class_name(Process*, oop klass);
  oop mm_class_get_compiled_class(Process*, oop klass);
  oop mm_class_dict(oop);

  oop mm_cfuns_to_funs_dict(Process*, oop cfuns_dict, oop imod);
  oop mm_new_slot_getter(Process*, oop imodule, oop owner, oop name, int idx);

  std::list<std::string> mm_sym_list_to_cstring_list(Process*, oop);

  CoreImage* core() { return _core_image; };

private:
  void type_check(oop, oop);
  // VM* _vm;
  CoreImage* _core_image;
};

#endif
