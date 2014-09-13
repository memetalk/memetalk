#ifndef PROCESS_HPP
#define PROCESS_HPP

#include <string>
#include <map>
#include <list>

#include "defs.hpp"

class VM;
class MMObj;
class ProcessControl;

class mm_rewind {
public:
  mm_rewind(oop ex) : mm_exception(ex) {}
  oop mm_exception;
};


class Process {
  enum {
    INVALID_STATE,
    RUN_STATE,
    STEP_INTO_STATE,
    STEP_OVER_STATE,
    STEP_OUT_STATE,
    HALT_STATE
  };
public:
  Process(VM*);

  oop run(oop, oop);

  VM* vm() { return _vm; }
  oop dp() { return get_dp(); }
  oop rp() { return get_rp(); }
  oop sp() { return _sp; }
  oop fp() { return _fp; }
  oop cp() { return _cp; }
  oop mp() { return _mp; }
  bytecode* ip() { return _ip; }

  oop get_rp() { if (_ep) { return ((oop*)_ep)[0]; } else { return _rp; } };
  oop get_dp() { if (_ep) { return ((oop*)_ep)[1]; } else { return _dp; } };

  // oop get_rp() { return _rp; };
  // oop get_dp() { return _dp; };

  MMObj* mmobj() { return _mmobj; }

  void stack_push(oop);
  void stack_push(word);
  void stack_push(bytecode*);

  oop unwind_with_exception(oop);

  oop do_call(oop, int*);
  oop do_call(oop, oop, int*);

  oop send_0(oop recv, oop selector, int* exc);
  oop send_1(oop recv, oop selector, oop arg, int* exc);
  oop send(oop recv, oop selector, oop args, int* exc);
  oop do_send(oop recv, oop selector, int num_args, int *exc);

  std::pair<oop,oop> lookup(oop, oop, oop);

  void raise(const char*, const char*);
  oop mm_exception(const char*, const char*);

  void halt_and_debug();
  void step_into();
  void step_over();
  void step_over_line();
  void step_out();

  word* stack() { return _stack; };
  unsigned int stack_depth();
  oop bp_at(unsigned int);

  void is_debugger(bool t) { _is_dbg = t; }; //to help filter logs
private:
  void pause() { _state = HALT_STATE; };

  void init();

  bool load_fun(oop, oop, oop, bool);

  void unload_fun_and_return(oop retval);

  oop stack_pop();
  int execute_primitive(std::string);
  void fetch_cycle(void*);

  void push_frame(number, number);
  void pop_frame();

  oop ctor_rdp_for(oop rp, oop cp);

  void setup_ep(oop, oop, oop);
  void copy_params_to_env(number, number);

  void dispatch(int, int);
  void handle_send(number);
  void handle_super_ctor_send(number);
  void handle_call(number);
  void handle_return(oop);
  void basic_new_and_load(oop);

  bool exception_has_handler(oop e, oop bp);

  void tick();
  void maybe_tick_call();
  void maybe_tick_return();

  bool _is_dbg;
  VM* _vm;
  MMObj* _mmobj;

  int _state;
  ProcessControl* _control;
  std::pair<Process*, oop> _dbg_handler;

  //this order is important: it reflects the order of registers
  //in the stack, and is used by bp_at()
  word* _fp;
  oop _cp;
  bytecode* _ip;
  oop _ep;
  oop _rp;
  oop _dp;
  oop _bp;
  //
  oop _mp;
  word* _sp;

  word* _stack;
  unsigned int _stack_depth;
  number _code_size;
  std::list<bytecode*> _volatile_breakpoints;
  oop _step_fp;
};

#endif
