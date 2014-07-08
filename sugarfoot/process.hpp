#ifndef PROCESS_HPP
#define PROCESS_HPP

#include <string>
#include "defs.hpp"

class VM;
class MMObj;

class Process {
public:
  Process(VM*);

  void run(oop, oop, oop);

  oop do_send(oop, oop, oop);
  void load_and_exec_fun(oop fun, number num_args, oop recv);
  void exec_fun(oop fun);
  void stack_push(oop);
  void stack_push(word);
  oop stack_pop();
  void execute_primitive(std::string);
  oop lookup(oop, oop);
  void fetch_cycle();

private:
  void init();

  VM* _vm;
  MMObj* _mmobj;

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
