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

  VM* vm() { return _vm; }
  oop dp() { return _dp; }
  oop rp() { return _rp; }
  oop sp() { return _sp; }
  oop fp() { return _fp; }
  oop cp() { return _cp; }
  MMObj* mmobj() { return _mmobj; }

  oop cp_index(int);


  void stack_push(oop);
  void stack_push(word);
  void stack_push(bytecode*);

  void unwind_with_exception(oop);

  oop do_send(oop, oop, oop);
  oop do_call(oop);

private:
  void init();
  void load_fun(oop, oop, oop, bool);

  void unload_fun_and_return(oop retval);

  oop stack_pop();
  int execute_primitive(std::string);
  void fetch_cycle(void*);

  void push_frame(number, number);
  void pop_frame();

  void setup_ep(oop);
  void copy_params_to_env(number, number);

  void dispatch(int, int);
  void handle_send(number);
  void handle_super_ctor_send(number);
  void handle_call(number);
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
  oop _bp;

  word* _fp;
  word* _sp;

  bytecode* _ip;

  word* _stack;

  number _code_size;
};

#endif
