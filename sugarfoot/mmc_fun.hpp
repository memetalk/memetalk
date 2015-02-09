#ifndef MMC_FUN
#define MMC_FUN

#include "defs.hpp"
#include <map>
#include <string>

class CoreImage;
class VM;
class MMObj;

class MMCFunction {
  static word MAGIC_NUMBER;
  static word HEADER_SIZE;
public:
  MMCFunction(VM*, CoreImage*, char* data, int size);
  oop load(Process*);
private:
  // void check_module_arity(oop module_arguments_list);
  // oop instantiate_class(oop, oop, oop, std::map<std::string, oop>&, oop);
  void load_header();
  void link_external_references();

  // void assign_module_arguments(oop imodule, oop module_arguments_list);
  // void load_default_dependencies_and_assign_module_arguments(oop imodule);

  // void load_aliases(oop imodule, oop aliases_dict, number num_params);
  // void create_alias_getters(oop imodule, oop imod_dict, oop aliases_dict, number num_params);

  // void  create_param_getters(oop imodule, oop imod_dict, oop params_list);

  VM* _vm;
  MMObj* _mmobj;
  CoreImage* _core_image;

  int _data_size;
  char* _data;

  //header
  word _ot_size;
  word _es_size;
  word _er_size;
  word _names_size;
  word _cfun_addr;
};


#endif
