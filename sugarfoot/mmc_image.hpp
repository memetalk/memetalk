#ifndef MMC_LOADER
#define MMC_LOADER

#include "defs.hpp"

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
  void load_header();
  void fix_external_references();

  VM* _vm;
  MMObj* _mmobj;
  CoreImage* _core_image;

  const char* _filepath;
  int _data_size;
  char* _data;

  //header
  word _ot_size;
  word _es_size;
  word _names_size;

  oop _compiled_module;
};


#endif
