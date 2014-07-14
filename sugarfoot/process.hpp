#ifndef PROCESS_HPP
#define PROCESS_HPP

#include <string>
#include <map>

#include "defs.hpp"

class VM;
class MMObj;

class Process {
public:
  Process(VM*);

  oop run(oop, oop, oop);


private:
  void init();
  void load_fun(oop, oop, oop, bool);
  oop do_send(oop, oop, oop);

  void unload_fun_and_return(oop retval);

  void stack_push(oop);
  void stack_push(word);
  void stack_push(bytecode*);
  oop stack_pop();
  void execute_primitive(std::string);
  void fetch_cycle(void*);

  void push_frame(number, number);
  void pop_frame();

  void dispatch(int, int);
  void handle_send(number);
  void handle_super_ctor_send(number);
  void basic_new_and_load();
  oop alloc_instance(oop klass);
  std::pair<oop,oop> lookup(oop, oop, oop);

  VM* _vm;
  MMObj* _mmobj;

  oop _mp;
  oop _cp;
  oop _rp;
  oop _dp;
  oop _ep;

  word* _fp;
  word* _sp;

  bytecode* _ip;

  word* _stack;

  number _code_size;
};

#endif
