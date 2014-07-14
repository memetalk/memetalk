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
  MMCImage(VM*, CoreImage*, const char*);
  oop load();
private:
  oop instantiate_module();
  oop instantiate_class(oop, oop, oop, std::map<std::string, oop>&, oop);
  void load_header();
  void link_external_references();

  VM* _vm;
  MMObj* _mmobj;
  CoreImage* _core_image;

  const char* _filepath;
  int _data_size;
  char* _data;

  //header
  word _ot_size;
  word _es_size;
  word _er_size;
  word _names_size;

  oop _compiled_module;
};


#endif
