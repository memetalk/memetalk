#include <stdlib.h>
#include <string>
#include <stdlib.h>
#include <map>
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

// int frame_count = 0;

oop Process::run(oop imod, oop recv, oop selector_sym) {
  init();
  return do_send(imod, recv, selector_sym);
}

void Process::init() {
  _stack = (word*) malloc(DEFAULT_STACK_SIZE);
  // debug() << " Initial stack " << _stack << endl;

  _sp = _stack; //stack
  _fp = _stack; //frame
  _ip = NULL; //instruction
  _mp = NULL; //module
  _cp = NULL; //context
  _rp = NULL; //receiver
  _dp = NULL; //receiver data
  _ep = NULL; //env
}

void Process::push_frame(number arity, number num_locals) {
  // debug() << "++ Push frame:  " << _sp << " num locals " << num_locals << endl;

  // oop curr_sp = _sp;

  stack_push(arity);

  word fp = (word) _fp;
  _fp = _sp;
  for (int i = 0; i < num_locals; i++) {
    stack_push((oop)0);
  }

  // debug() << "push_frame [fm:" << frame_count++ << "]  arity: " << arity << " num locals: " << num_locals
  //         << " SP:" << curr_sp
  //         << " fp: " << (oop) fp << " cp: " << _cp << " ip: " << _ip << " ep: " << _ep
  //         << " rp: " << _rp << " dp: " << _dp << " mp: " << _mp << endl;

  stack_push(fp);
  stack_push(_cp);
  stack_push(_ip);
  stack_push(_ep);
  stack_push(_rp);
  stack_push(_dp);
  // debug() << " push frame SP is: " << _sp << endl;
}

void Process::pop_frame() {
  number arity = *_fp;
  word fp = (word) _fp;

  // debug() << "pop_frame begin SP: " << _sp << endl;

  _dp = stack_pop();
  _rp = stack_pop();
  _ep = stack_pop();
  _ip = (bytecode*) stack_pop();
  _cp = stack_pop();
  _fp = stack_pop();

  // debug() << "pop_frame [fm:" << --frame_count << " arity: " << arity
  //         << " fp: " << _fp << " cp: " << _cp << " ip: " << _ip << " ep: " << _ep
  //         << " rp: " << _rp << " dp: " << _dp << " mp (before): " << _mp << endl;

  if (_cp) {// first frame has _cp = null
    // debug() << "pop_frame: getting module from fun " << _cp << endl;
    _mp = _mmobj->mm_function_get_module(_cp);
    // debug() << "got mp:" << _mp << endl;
  }

  _sp = (oop) fp - (arity + 1);
  // debug() << "-- pop frame SP:" <<  _sp << endl;
}

void Process::unload_fun_and_return(oop retval) {
  pop_frame();
  stack_push(retval);
}

oop Process::alloc_instance(oop klass) {
  if (klass == NULL) {
    // debug() << "alloc_instance: klass is null" << endl;
    return NULL;
  }

  // debug() << "alloc_instance for klass " << klass << endl;
  // debug() << "alloc_instance: class name: " << _mmobj->mm_string_cstr(_mmobj->mm_class_name(klass)) << endl;

  number payload = _mmobj->mm_behavior_size(klass);
  if (payload == INVALID_PAYLOAD) {
    bail("new_delegate: Received flagged/behavior payload");
  }

  // debug() << "payload " << payload << endl;

  oop instance = (oop) calloc(sizeof(word), payload + 2); //2: vt, delegate
  // debug() << "new instance [size: " << payload << "]: "
  //         << _mmobj->mm_string_cstr(_mmobj->mm_class_name(klass)) << " = " << instance << endl;

  oop klass_parent =  _mmobj->mm_object_delegate(klass);

  ((oop*)instance)[0] = klass;
  ((oop*) instance)[1] = alloc_instance(klass_parent);

  // debug() << "Created recursive delegate " << instance << endl;
  return instance;
}

void Process::basic_new_and_load() {
  oop klass = _rp;
  oop instance = alloc_instance(klass);
  _rp = instance;
  _dp = instance;
}

void Process::load_fun(oop recv, oop drecv, oop fun, bool should_allocate) {
  // loads frame if necessary and executes fun

  // debug() << " load_fun rec:" << recv << " drec:" << drecv << " fun:" << fun << endl;

  if (_mmobj->mm_function_is_getter(fun)) {
    number idx = _mmobj->mm_function_access_field(fun);
    stack_push(((oop*)recv)[idx]);
    return;
  }

  push_frame(_mmobj->mm_function_get_num_params(fun),
             _mmobj->mm_function_get_num_locals(fun));

  _rp = recv;
  _dp = drecv;
  _cp = fun;
  _mp = _mmobj->mm_function_get_module(_cp);

  // debug() << "load_fun: module for fun " << fun << " is " << _mp << endl;

  // debug() << "load_fun: is ctor? " << _mmobj->mm_function_is_ctor(fun) << " alloc? " << should_allocate << endl;

  if (_mmobj->mm_function_is_ctor(fun) and should_allocate) {
    basic_new_and_load();
  }

  if (_mmobj->mm_function_is_prim(fun)) {
    oop prim_name = _mmobj->mm_function_get_prim_name(fun);
    std::string str_prim_name = _mmobj->mm_string_cstr(prim_name);
    execute_primitive(str_prim_name);
  } else {
    _ip = _mmobj->mm_function_get_code(fun);
    // debug() << "first instruction " << decode_opcode(*_ip) << endl;
    _code_size = _mmobj->mm_function_get_code_size(fun);
  }
}

oop Process::do_send(oop imod, oop recv, oop selector) {
  std::pair<oop, oop> res = lookup(recv, * (oop*) recv, selector);

  oop drecv = res.first;
  oop fun = res.second;
  if (!fun) {
    bail("do_send: lookup failed"); //todo
  }

  load_fun(recv, drecv, fun, true);
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
    if (_ip != 0) { // the bottommost frame has ip = 0
      _ip++; //this can't be done after dispatch, where an ip for a different fun may be set
             //(thus, doing so would skip the first instruction)
    }
    int opcode = decode_opcode(code);
    int arg = decode_args(code);
    // debug() << " [dispatching] " << opcode << endl;
    dispatch(opcode, arg);
    // debug() << " [end of dispatch] " << opcode << endl;
  }
}

void Process::handle_send(number num_args) {
  oop selector = stack_pop();
  oop recv = stack_pop(); //(oop) * _sp;

  // debug() << " SEND " << selector << " -- " << _mmobj->mm_symbol_cstr(selector) << endl;

  std::pair<oop,oop> res = lookup(recv, * (oop*) recv, selector);
  oop drecv = res.first;
  oop fun = res.second;
  // debug() << "Lookup FOUND " << fun << endl;

  if (fun == NULL) {
    bail("Selector not found");
  }

  number arity = _mmobj->mm_function_get_num_params(fun);
  if (num_args != arity) {
    debug() << num_args << " != " << arity << endl;
    bail("arity and num_args differ");
  }
  load_fun(recv, drecv, fun, true);
}

void Process::handle_super_ctor_send(number num_args) {
  oop selector = stack_pop();
  // debug() << " SUPER: " << selector << " -- " << _mmobj->mm_symbol_cstr(selector) << endl;

  // lookup starts at the parent of rp's class
  oop instance = _dp;
  oop klass = * (oop*) instance;
  oop pklass = _mmobj->mm_object_delegate(klass);
  oop receiver = _mmobj->mm_object_delegate(instance);

  std::pair<oop, oop> res = lookup(receiver, * (oop*) pklass, selector);
  oop drecv = res.first;
  oop fun = res.second;

  if (fun == NULL) {
    bail("Selector not found");
  }

  debug() << "Lookup FOUND " << fun << endl;


  number arity = _mmobj->mm_function_get_num_params(fun);
  if (num_args != arity) {
    debug() << num_args << " != " << arity << endl;
    bail("arity and num_args differ");
  }

  load_fun(_rp, drecv, fun, false);
}

void Process::dispatch(int opcode, int arg) {
    // debug() << " executing " << opcode << " " << arg << endl;
    oop val;
    switch(opcode) {
      case PUSH_LOCAL:
        debug() << "PUSH_LOCAL " << arg << " " << (oop) *(_fp + arg + 1) << endl;
        stack_push(*(_fp + arg + 1));
        break;
      case PUSH_PARAM:
        debug() << "PUSH_PARAM " << arg << " " << (oop) *(_fp - ((number) *_fp - arg)) << endl;
        stack_push(*(_fp - ((number) *_fp - arg)));
        break;
      case PUSH_LITERAL:
        debug() << "PUSH_LITERAL " << arg << " " << _mmobj->mm_function_get_literal_by_index(_cp, arg) << endl;
        stack_push(_mmobj->mm_function_get_literal_by_index(_cp, arg));
        break;
      case PUSH_MODULE:
        stack_push(_mp);
        break;
      case PUSH_FIELD:
        debug() << "PUSH_FIELD " << arg << " " << (oop) *(_dp + arg + 2) << endl;
        stack_push(*(_dp + arg + 2));
        break;
      case PUSH_THIS:
        debug() << "PUSH_THIS " << arg << " " << _rp << endl;
        stack_push(_rp);
        break;
      case RETURN_TOP:
        val = stack_pop();
        debug() << "RETURN_TOP " << arg << " " << val << endl;
        unload_fun_and_return(val);
        break;
      case RETURN_THIS:
        debug() << "RETURN_THIS " << _rp << endl;
        unload_fun_and_return(_rp);
        break;
      case POP:
        val =stack_pop();
        debug() << "POP " << arg << " = " << val << endl;
        break;
      case POP_LOCAL:
        val = stack_pop();
        debug() << "POP_LOCAL " << arg << " on " << (oop) (_fp + arg + 1) << " -- "
                << (oop) *(_fp + arg + 1) << " = " << val << endl;
        *(_fp + arg + 1) = (word) val;
        break;
      case POP_FIELD:
        val = stack_pop();
        debug() << "POP_FIELD " << arg << " on " << (oop) (_dp + arg + 2) << " -- "
                << (oop) *(_dp + arg + 2) << " = " << val << endl; //2: vt, delegate
        *(_dp + arg + 2) = (word) val;
        break;
      case SEND:
        handle_send(arg);
        break;
      case SUPER_CTOR_SEND:
        handle_super_ctor_send(arg);
        break;
      // case SUPER_SEND:
      //   handle_super_send(arg);
      //   break;
      // case RETURN_THIS:
      //   break;
      default:
        debug() << "Unknown opcode " << opcode << endl;
        bail("opcode not implemented");
    }
}

std::pair<oop, oop> Process::lookup(oop drecv, oop vt, oop selector) {
  // assert( *(oop*) selector == _core_image->get_prime("Symbol"));
  if (vt == NULL) {
    bail("lookup FAILED!!!"); //todo: raise
  }

  oop dict = _mmobj->mm_behavior_get_dict(vt);
  if (_mmobj->mm_dictionary_has_key(dict, selector)) {
    oop fun = _mmobj->mm_dictionary_get(dict, selector);
    // debug() << "Lookup of " << selector << " found in " << vt << " fun is: " << fun << endl;
    std::pair<oop,oop> res = std::pair<oop,oop>(drecv, fun);
    return res;
  } else {
    // debug() << "Lookup of " << selector << " NOT found in " << vt << ", recursively looking up..." << endl;
    oop pklass = _mmobj->mm_object_delegate(vt);
    oop delegate = _mmobj->mm_object_delegate(drecv);
    return lookup(delegate, pklass, selector);
  }
}

oop Process::stack_pop() {
  oop val = * (oop*)_sp;
  _sp--;
  // debug() << "     POP " << val << " >> " << _sp << endl;
  return val;
}

void Process::stack_push(oop data) {
  _sp++;
  // debug() << "     PUSH " << data << " -> " << _sp << endl;
  * (word*) _sp = (word) data;
}

void Process::stack_push(word data) {
  _sp++;
  // debug() << "     PUSH " << data << " -> " << _sp << endl;
  * (word*) _sp = data;
}

void Process::stack_push(bytecode* data) {
  _sp++;
  // debug() << "     PUSH " << data << " -> " << _sp << endl;
  * (word*) _sp = (word) data;
}


void Process::execute_primitive(std::string name) {
  _vm->get_primitive(name)(this);
}
