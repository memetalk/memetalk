#ifndef PROCESS_HPP
#define PROCESS_HPP

#include <string>
#include "defs.hpp"

class VM;
class MMObj;

class Process {
public:
  Process(VM*);

  oop run(oop, oop, oop);

  oop do_send(oop, oop, oop);

  void load_fun(oop fun, oop recv);
  void unload_fun_and_return(oop retval);

  void stack_push(oop);
  void stack_push(word);
  void stack_push(bytecode*);
  oop stack_pop();
  void execute_primitive(std::string);
  oop lookup(oop, oop);
  void fetch_cycle(void*);

  void push_frame(number, number);
  void pop_frame();

private:
  void init();
  void dispatch(int, int);
  void handle_send(number);
  void basic_new_and_load();

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
