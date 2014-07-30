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
  VM(int argc, char** argv, const char* core_img_filepath);

  int& argc() { return _argc; };
  char** argv() { return _argv; };

  MMObj* mmobj();

  int start();
  oop new_symbol(const char*);
  oop new_symbol(oop);

  void register_primitive(std::string, prim_function_t);

  prim_function_t get_primitive(std::string);

  oop instantiate_module(char* name, oop module_args_list);

  oop get_prime(const char* name);

  CoreImage* core() { return _core_image; };

  Process* process() { return _process; };

private:
  void dump_prime_info();
  void dictionary_dump(oop dict);

  int _argc;
  char** _argv;
  CoreImage* _core_image;
  MMObj* _mmobj;
  Process* _process;

  std::map<std::string, oop> _symbols;

  std::map<std::string, prim_function_t> _primitives;
};

#endif
