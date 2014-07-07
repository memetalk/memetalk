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

  void init_stack();
  oop do_send(oop, oop, oop);
  void load_and_exec_fun(oop fun, number num_args, oop recv);
  void exec_fun(oop fun);
  void stack_push(oop);
  void stack_push(word);
  oop stack_pop();
  void execute_primitive(const std::string&);
  oop lookup(oop, oop);
  void fetch_cycle();

private:

  CoreImage* _core_image;

  std::map<std::string, oop> _symbols;

  //registers
  number _bp;
  oop _sp;
  oop _ip;
  oop _fp;
  oop _mp;
  oop _cp;
  oop _rp;
  oop _dp;
  oop _ep;

  word* _stack;
};

#endif
