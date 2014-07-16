#ifndef VM_HPP
#define VM_HPP

#include "defs.hpp"
#include <map>
#include <string>


class CoreImage;
class MMObj;
class Process;

class VM {

public:
  VM(const char* core_img_filepath);

  MMObj* mmobj();

  int start(char* filepath);
  oop new_symbol(const char*);

  void register_primitive(std::string, prim_function_t);

  prim_function_t get_primitive(std::string);

  oop instantiate_module(char* name, oop module_args_list);

private:
  void dump_prime_info();
  void dictionary_dump(oop dict);

  CoreImage* _core_image;
  MMObj* _mmobj;
  Process* _process;

  std::map<std::string, oop> _symbols;

  std::map<std::string, prim_function_t> _primitives;
};

#endif
