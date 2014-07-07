#include "vm.hpp"
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <string.h>
#include "core_loader.hpp"
#include "mmc_loader.hpp"
#include "report.hpp"
#include "defs.hpp"
#include "mmobj.hpp"

VM::VM(const char* core_img_filepath)
  : _core_image(new CoreImage(core_img_filepath)) {
}

int VM::start(char* filepath) {
  _core_image->load();

  MMCImage* mmc = new MMCImage(this, _core_image, filepath);
  oop imod = mmc->load();

  init_stack();
  do_send(imod, imod, new_symbol("main"));

  return 0;
}

oop VM::new_symbol(const char* cstr) {
  std::string s = cstr;
  if (_symbols.find(s) == _symbols.end()) {
    _symbols[s] = mm_symbol_new(cstr, _core_image);
  }
  return _symbols[s];
}

void VM::init_stack() {
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

void VM::load_and_exec_fun(oop fun, number num_args, oop recv) {
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
  _mp = mm_function_get_module(fun);
  _fp = _sp;

  exec_fun(fun);
}


void VM::exec_fun(oop fun) {
  if (!mm_function_is_prim(fun)) {
    bail("Only primitive functions supported");
  }

  oop prim_name = mm_function_get_prim_name(fun);
  std::string str_prim_name = mm_string_cstr(prim_name);
  debug() << "Executing fun " << str_prim_name << std::endl;
  execute_primitive(str_prim_name);
}

oop VM::do_send(oop imod, oop recv, oop selector_sym) {
  oop vt =  * (oop*) recv;
  oop fun = lookup(vt, selector_sym);
  if (!fun) {
    bail("lookup failed"); //todo
  }

  load_and_exec_fun(fun, 0, recv);
  fetch_cycle();
  return stack_pop();
}

void VM::fetch_cycle() {
}

oop VM::lookup(oop vt, oop selector) {
  oop dict = mm_behavior_get_dict(vt);
  if (mm_dictionary_has_key(dict, selector)) {
    return mm_dictionary_get(dict, selector);
  }
  return NULL;
}

oop VM::stack_pop() {
  oop val = (oop) _stack[--_bp];
  return val;
}

void VM::stack_push(oop data) {
  _stack[_bp++] = (word) data;
}

void VM::stack_push(word data) {
  _stack[_bp++] = data;
}

void VM::execute_primitive(const std::string&) {
}

//

int main(int argc, char** argv) {
  if (argc != 2) {
    bail("usage: sf-vm <file.mmc>");
  }
  return VM("core.img").start(argv[1]);
}
