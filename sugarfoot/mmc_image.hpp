#ifndef MMC_LOADER
#define MMC_LOADER

#include "defs.hpp"
#include <map>
#include <string>

class CoreImage;
class VM;
class MMObj;

class MMCImage {
  static word MAGIC_NUMBER;
  static word HEADER_SIZE;
public:
  MMCImage(VM*, CoreImage*, const std::string&);
  oop load();
  oop instantiate_module(oop);
private:
  oop instantiate_class(oop, oop, oop, std::map<std::string, oop>&, oop);
  void load_header();
  void link_external_references();
  void link_exception_types();

  void assign_module_arguments(oop imodule, oop module_arguments_list);
  void load_default_dependencies_and_assign_module_arguments(oop imodule);
  void  create_param_getters(oop imodule, oop imod_dict, oop params_list);

  VM* _vm;
  MMObj* _mmobj;
  CoreImage* _core_image;

  std::string _filepath;
  int _data_size;
  char* _data;

  //header
  word _ot_size;
  word _es_size;
  word _et_size;
  word _er_size;
  word _names_size;

  oop _compiled_module;
};


#endif
