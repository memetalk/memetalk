#ifndef MMC_LOADER
#define MMC_LOADER

#include "defs.hpp"

class CoreImage;

class MMCImage {
  static word MAGIC_NUMBER;
  static word HEADER_SIZE;
public:
  MMCImage(CoreImage*, const char*);
  void load();
  oop instantiate_module();
private:
  void load_header();
  void fix_external_references();

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
