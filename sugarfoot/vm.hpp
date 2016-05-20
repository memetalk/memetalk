#ifndef VM_HPP
#define VM_HPP

#include "defs.hpp"
#include "log.hpp"
#include <map>
#include <string>
#include <list>


class CoreImage;
class MMCImage;
class MMObj;
class Process;

class VM {

public:
  VM(int argc, char** argv, bool online, const char* core_img_filepath);

  int& argc() { return _argc; };
  char** argv() { return _argv; };

  bool running_online() { return _online; };

  MMObj* mmobj();


  int start();
  oop new_symbol(const char*);
  oop new_symbol(Process*, oop);

  void register_primitive(std::string, prim_function_t);

  prim_function_t get_primitive(Process*, std::string);

  oop instantiate_module(Process*, const char* name, oop module_args_list);

  oop get_prime(const char* name);

  CoreImage* core() { return _core_image; };

  // Process* process() { return _process; };

  oop compile_fun(Process*, const char* text, std::list<std::string>, oop cmod, int*);
  oop recompile_fun(Process*, oop cfun, const char* text, int* exc);

  std::pair<Process*, oop> start_debugger(Process* target);

  void bail(const std::string& msg);
  void bail();

  void print_error(Process* proc, oop retval);

  oop get_compiled_module(Process* proc, std::string name);

private:
  // void dump_prime_info();
  // void dictionary_dump(oop dict);
  void print_retval(Process*, oop retval);
  Process* init();

  MMLog _log;
  int _argc;
  char** _argv;
  bool _online;
  CoreImage* _core_image;
  MMObj* _mmobj;
//  std::list<Process*> _processes;

  std::map<std::string, oop> _symbols;

  std::map<std::string, prim_function_t> _primitives;
  std::map<std::string, MMCImage*> _modules;
};

#endif
