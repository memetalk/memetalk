#include <stdlib.h>
#include <string>
#include <stdlib.h>
#include "process.hpp"
#include "defs.hpp"
#include "vm.hpp"
#include "core_image.hpp"
#include "mmc_image.hpp"
#include "report.hpp"
#include "mmobj.hpp"
#include "utils.hpp"

Process::Process(VM* vm)
  : _vm(vm), _mmobj(vm->mmobj()) {
}

oop Process::run(oop imod, oop recv, oop selector_sym) {
  init();
  return do_send(imod, recv, selector_sym);
}

void Process::init() {
  _stack = (word*) malloc(DEFAULT_STACK_SIZE);

  _sp = _stack -1; //stack
  _fp = _stack; //frame
  _ip = NULL; //instruction
  _mp = NULL; //module
  _cp = NULL; //context
  _rp = NULL; //receiver
  _dp = NULL; //receiver data
  _ep = NULL; //env
}

void Process::push_frame(number num_args, number num_locals) {
  stack_push(_fp);
  stack_push(_cp);
  stack_push(_ip);
  stack_push(_ep);
  stack_push(num_args + num_locals);
  stack_push(_rp);
  stack_push(_dp);
  for (int i = 0; i < num_args; i++) {
    stack_push((oop)0);
  }
  _fp = _sp;
  for (int i = 0; i < num_locals; i++) {
    stack_push((oop)0);
  }
}

void Process::pop_frame() {
  number locals_size = (number) _fp[-2];

  _sp -= locals_size; // purge local space

  _dp = stack_pop();
  _rp = stack_pop();
  /* num_args =  (number)*/ stack_pop();
  _ep = stack_pop();
  _ip = (bytecode*) stack_pop();
  _cp = stack_pop();
  _fp = stack_pop();
  if (_cp) {
    _mp = _mmobj->mm_function_get_module(_cp);
  }
}

void Process::unload_fun_and_return(oop retval) {
 // 664         # clean up locals
 // 665         if len(self.stack) == 0:
 // 666             return retval
  // debug() << "unload and return " << retval << endl;
  pop_frame();
  stack_push(retval);
}

void Process::load_fun(oop fun, oop recv) {
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

  push_frame(_mmobj->mm_function_get_num_params(fun),
             _mmobj->mm_function_get_num_locals(fun));

  _rp = recv;
  _dp = recv;
  _cp = fun;
  _mp = _mmobj->mm_function_get_module(_cp);

  if (_mmobj->mm_function_is_prim(fun)) {
    oop prim_name = _mmobj->mm_function_get_prim_name(fun);
    std::string str_prim_name = _mmobj->mm_string_cstr(prim_name);
    execute_primitive(str_prim_name);
  } else {
    _ip = _mmobj->mm_function_get_code(fun);
    _code_size = _mmobj->mm_function_get_code_size(fun);
  }
}

oop Process::do_send(oop imod, oop recv, oop selector_sym) {
  oop vt =  * (oop*) recv;
  oop fun = lookup(vt, selector_sym);
  if (!fun) {
    bail("lookup failed"); //todo
  }

  load_fun(fun, recv);
  fetch_cycle((void*) -1);
  oop val = stack_pop();
  return val;
}

void Process::fetch_cycle(void* stop_at_fp) {
  bytecode* start_ip = _ip;
  while (_ip) { // /*_fp > stop_at_fp &&*/ ((_ip - start_ip) * sizeof(bytecode))  < _code_size) {
    // debug() << "fp " << _fp << " stop " <<  stop_at_fp << " ip-start " <<  (_ip - start_ip)  << " codesize " <<  _code_size <<
    //   "ip " << _ip << std::endl;
    bytecode code = *_ip;
    int opcode = decode_opcode(code);
    int arg = decode_args(code);
    dispatch(opcode, arg);

    if (_ip != 0) { // the bottommost frame has ip = 0
      _ip++;
    }
  }
  // debug() << "fetch_cycle end\n";
 // 571         while self.fp > stop_at_fp and self.ip < len(self.code):
 // 572             pack32 = self.code[self.ip]
 // 573             op,arg = opcode.decode_op(pack32)
 // 574             handler = opcode.handler(op)
 // 575             self.ip = self.ip + 1
 // 576             print handler,arg
 // 577             globals()[handler](self,arg)
 // 578             self.print_frames()
 // 579             print " ",self.stack
}

void Process::dispatch(int opcode, int arg) {
    debug() << " executing " << opcode << " " << arg << endl;
    switch(opcode) {
      case PUSH_LOCAL:
        stack_push(*(_fp + arg));
      case PUSH_LITERAL:
        stack_push(_mmobj->mm_function_get_literal_by_index(_cp, arg));
        break;
      case RETURN_TOP:
        unload_fun_and_return(stack_pop());
        break;
      case POP_LOCAL:
        *(_fp + arg) = (word) stack_pop();
        break;
      // case RETURN_THIS:
      //   break;
      default:
        debug() << "Unknown opcode " << opcode << endl;
        bail("opcode not implemented");
    }
}

oop Process::lookup(oop vt, oop selector) {
  oop dict = _mmobj->mm_behavior_get_dict(vt);
  if (_mmobj->mm_dictionary_has_key(dict, selector)) {
    return _mmobj->mm_dictionary_get(dict, selector);
  }
  return NULL;
}

oop Process::stack_pop() {
  oop val = * (oop*)_sp;
  _sp--;
  return val;
}

void Process::stack_push(oop data) {
  _sp++;
  * (word*) _sp = (word) data;
}

void Process::stack_push(word data) {
  _sp++;
  * (word*) _sp = data;
}

void Process::stack_push(bytecode* data) {
  _sp++;
  * (word*) _sp = (word) data;
}


void Process::execute_primitive(std::string name) {
  _vm->get_primitive(name)(this);
}
