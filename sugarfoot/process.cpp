#include "process.hpp"
#include "defs.hpp"
#include "vm.hpp"
#include <stdlib.h>
#include <string>
#include "core_image.hpp"
#include "mmc_image.hpp"
#include "report.hpp"
#include "mmobj.hpp"

Process::Process(VM* vm)
  : _vm(vm), _mmobj(vm->mmobj()) {
}

void Process::run(oop imod, oop recv, oop selector_sym) {
  init();
  do_send(imod, recv, selector_sym);
}

void Process::init() {
  _bp = 0;
  _sp = NULL; //stack
  _ip = NULL; //instruction
  _fp = NULL; //frame
  _mp = NULL; //module
  _cp = NULL; //context
  _rp = NULL; //receiver
  _dp = NULL; //receiver data
  _ep = NULL; //env
  _stack = (word*) malloc(DEFAULT_STACK_SIZE);
}

void Process::load_and_exec_fun(oop fun, number num_args, oop recv) {
  // loads frame if necessary and executes fun

  //if fun is accessor/mutator
    //if fun is getter:
    //  ... return
    //elif fun is setter:
    // ... return
    //elif: internal error: should be either getter or setter
  // else
    // setup frame
    // setup registers
    // execute fun

  stack_push(_fp);
  stack_push(_cp);
  stack_push(_ip);
  stack_push(_ep);
  stack_push(num_args);
  stack_push(_rp);
  stack_push(_dp);

  _rp = recv;
  _dp = recv;
  _mp = _mmobj->mm_function_get_module(fun);
  _fp = _sp;

  exec_fun(fun);
}


void Process::exec_fun(oop fun) {
  if (!_mmobj->mm_function_is_prim(fun)) {
    bail("Only primitive functions supported");
  }

  oop prim_name = _mmobj->mm_function_get_prim_name(fun);
  std::string str_prim_name = _mmobj->mm_string_cstr(prim_name);
  execute_primitive(str_prim_name);
}

oop Process::do_send(oop imod, oop recv, oop selector_sym) {
  oop vt =  * (oop*) recv;
  oop fun = lookup(vt, selector_sym);
  if (!fun) {
    bail("lookup failed"); //todo
  }

  load_and_exec_fun(fun, 0, recv);
  fetch_cycle();
  return stack_pop();
}

void Process::fetch_cycle() {
}

oop Process::lookup(oop vt, oop selector) {
  oop dict = _mmobj->mm_behavior_get_dict(vt);
  if (_mmobj->mm_dictionary_has_key(dict, selector)) {
    return _mmobj->mm_dictionary_get(dict, selector);
  }
  return NULL;
}

oop Process::stack_pop() {
  oop val = (oop) _stack[--_bp];
  return val;
}

void Process::stack_push(oop data) {
  _stack[_bp++] = (word) data;
}

void Process::stack_push(word data) {
  _stack[_bp++] = data;
}


void Process::execute_primitive(std::string name) {
  _vm->get_primitive(name)(this);
}
