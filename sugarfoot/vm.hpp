#ifndef VM_HPP
#define VM_HPP

#include "defs.hpp"
#include <map>
#include <string>


class CoreImage;

class VM {

public:
  VM(const char* core_img_filepath);
  int start(char* filepath);
  oop new_symbol(const char*);

private:

  CoreImage* _core_image;

  std::map<std::string, oop> _symbols;
};

#endif
