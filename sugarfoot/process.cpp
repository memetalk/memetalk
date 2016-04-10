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
#include "ctrl.hpp"
#include <sstream>
#include <assert.h>

Process::Process(VM* vm)
  :   _is_dbg(false), _vm(vm), _mmobj(vm->mmobj()), _control(new ProcessControl()), _dbg_handler(NULL, MM_NULL) {
  init();
  _state = RUN_STATE;
  _step_bp = MM_NULL;
}

oop Process::run(oop recv, oop selector_sym, int* exc) {
  //atm, we don't need to check exc
  //since ::unwind_with_exception will terminate the vm
  //if there is no more stack to unwind.
  return send_0(recv, selector_sym, exc);
}

void Process::init() {
  _stack = (word*) malloc(DEFAULT_STACK_SIZE);
  // dbg() << " Initial stack " << _stack << endl;
  _stack_depth = 0;
  _state = INVALID_STATE;
  _sp = _stack; //stack
  _fp = NULL;
  _bp = NULL; //base
  _ip = NULL; //instruction
  _mp = NULL; //module
  _cp = NULL; //context
  _ss = 0; //local storage
}

oop Process::get_arg(number idx) {
  // std::cerr << "Process::get_arg " << idx << " fp: " << _fp << " *:" << * ((oop*)_fp + idx) << endl;
  return * ((oop*)_fp + idx);
};

oop Process::set_rp(oop rp) {
  return * (oop*) (_fp + _ss) = rp;
}

oop Process::set_dp(oop dp) {
  return * (oop*) (_fp + _ss + 1) = dp;
}

oop Process::rp() {
  // dbg() << "Process::rp  fp:" <<  _fp << " _ss:" << _ss << " ==> " << * (oop*) (_fp + _ss) << endl;
  return * (oop*) (_fp + _ss );
}

oop Process::dp() {
  return * (oop*) (_fp + _ss + 1);
}

oop Process::cp_from_frame(oop bp) {
  return * ((oop*)bp - 3);
}

bytecode* Process::ip_from_frame(oop bp) {
  return (bytecode*) * ((oop*)bp - 2);
}

void Process::push_frame(oop recv, oop drecv, number arity, number storage_size) {
  // dbg() << "++ Push frame:  " << _sp << " num locals " << num_locals << endl;

  // oop curr_sp = _sp;
  _stack_depth++;

  //BEGIN stack frame layout
  //---- if you change this, change pop_frame() and the frame accessor methods
  //---- as well!!

  oop fp = _fp;
  _fp = _sp - (arity - 1);  // _sp points to the last arg pushed

  dbg() << "push_frame before -- sp: " << _sp << " old fp: " << fp << " new fp:" << _fp << endl;

  for (int i = 0; i < storage_size - arity; i++) {
    stack_push(MM_NULL);
  }


  stack_push(recv);
  stack_push(drecv);

  stack_push(fp);
  stack_push(_cp);
  stack_push(_ip);
  stack_push(_ss); //storage size
  stack_push(_bp);

  //END of frame layout

  _bp = _sp;

  _ss = storage_size;

  dbg() << "push_frame result: arity: " << arity << " storage: " << storage_size
        << " fp: " << _fp << " bp: " << _bp << " cp: " << _cp << " ip: " << _ip
        << " mp: " << _mp << endl;

  // //logging
  // if (!_is_dbg) {
  //   if (_cp) {
  //     oop name = _mmobj->mm_function_get_name(_cp);
  //     char* str_name = _mmobj->mm_string_cstr(name);
  //     std::cerr << "FRAME: " << str_name << " FP" << _fp << " BP:" << _bp
  //               << " CP: " << _cp << " IP: " << _ip << endl;
  //   } else {
  //     std::cerr << "FRAME FP:" << _fp << " BP:" << _bp
  //               << " CP: " << _cp << " IP: " << _ip << endl;
  //   }
  // }

  // dbg() << " push frame SP is: " << _sp << endl;
}

void Process::pop_frame() {
  number storage_size = _ss;

  // dbg() << "pop_frame begin SP: " << _sp << endl;

  _stack_depth--;

  _sp = _bp; //restore sp, ignoring frame data. push_frame/pop_frame are always in sync.

  _bp = stack_pop();
  _ss = (number) stack_pop();
  _ip = (bytecode*) stack_pop();
  _cp = stack_pop();
  _fp = stack_pop();

  dbg() << "pop_frame: fp: " << _fp << endl;

  _sp = _sp - (storage_size + 2); //2: fp, dp

  dbg() << "pop_frame [fm:" << _stack_depth << " storage_size: " << _ss
        << " sp: " << _sp << " fp: " << _fp << " bp: " << _bp << " cp: " << _cp << " ip: " << _ip
        << " mp (before): " << _mp << endl;

  if (_cp) {// first frame has _cp = null
    // dbg() << "pop_frame: getting module from fun " << _cp << endl;
    _mp = _mmobj->mm_function_get_module(this, _cp, true);
    dbg() << "MP for fun unloaded: " << _mp << endl;
  } else {
    dbg() << "++ MP for fun unloaded is null!" << endl;
  }
}

void Process::unload_fun_and_return(oop retval) {
  pop_frame();
  stack_push(retval);
}

oop Process::ctor_rdp_for(oop rp, oop cp) {
  dbg() << "ctor_rdp_for rp: " << rp << ", cp: " << cp << endl;
  if (!rp) {
    bail("no rdp for ctor!");
  }
  oop vt = _mmobj->mm_object_vt(rp);
  oop cclass = _mmobj->mm_class_get_compiled_class(this, vt, true);

  oop other_cclass = _mmobj->mm_function_get_owner(this, cp, true);

  dbg() << "rdp_for_ctor cclass: " << cclass << ", other cclass: " << other_cclass << endl;

  if (cclass == other_cclass) {
    return rp;
  } else {
    return ctor_rdp_for(_mmobj->mm_object_delegate(rp), cp);
  }
}

void Process::basic_new_and_load(oop klass) {
  oop rp = _mmobj->alloc_instance(this, klass);
  oop dp = ctor_rdp_for(rp, _cp);
  dbg() << "basic_new: " << rp << " dp: " << dp << endl;
  set_rp(rp);
  set_dp(dp);
}

void Process::setup_fp(number params, number storage_size) {
  //ideally this should be a calloc followed by memcpy
  oop fp = (oop) calloc(sizeof(oop), storage_size + 2); //+2: space for rp, dp
  dbg() << "Process::setup_fp allocated fp: " << fp << " - " << " params: " << params
        << " storage_size: " << storage_size << endl;

  for (int i = 0; i < storage_size + 2; i++) {
    dbg() << "Process::setup_fp new_fp[" << i << " - " << (oop*)fp << "]  " << *((oop*)_fp + i) << endl;
    ((oop*)fp)[i] = *((oop*)_fp + i);
  }

  // for (int j = 0, i = params; i > 0; i--, j++) { //copying parameters
  //   ((oop*)fp)[j] = *((oop*)_fp - 2 - i); //-2: rp,dp
  // }
  // ((oop*)fp)[storage_size] =  *((oop*)_fp + storage_size); //rp
  // ((oop*)fp)[storage_size+1] =*((oop*)_fp + storage_size + 1); //dp
  // ((oop*)fp)[storage_size+2] = (oop) params;

  _fp = fp;
}

void Process::restore_fp(oop fp, number params, number env_offset) {
  dbg() << "Process::restore_fp " << fp << " " << params << " " << env_offset << endl;

  for (int i = 0; i < params; i++) {
    dbg() << "fp[" << env_offset + i << "] = " << * (oop*)(_fp + i) << endl;
    ((oop*)fp)[env_offset + i] = * (oop*)(_fp + i);
  }
  _fp = fp;
}

bool Process::load_fun(oop recv, oop drecv, oop fun, bool should_allocate) {
  if (!(_mmobj->mm_is_function(fun) || _mmobj->mm_is_context(fun))) {
    raise("TypeError", "Expecting function or context");
  }

  dbg() << "Process::load_fun " << fun << endl;

  if (_mmobj->mm_function_is_getter(this, fun, true)) {
    number idx = _mmobj->mm_function_access_field(this, fun, true);
    dbg() << "GETTER: idx " << idx << " on " << recv << endl;
    oop val = ((oop*)drecv)[idx];
    dbg() << "GETTER: pushing retval: " << val << endl;
    stack_push(val);
    return false;
  }

  number num_params = _mmobj->mm_function_get_num_params(this, fun, true);
  number storage_size = _mmobj->mm_function_get_num_locals_or_env(this, fun, true);
  push_frame(recv, drecv, num_params, storage_size);


  dbg() << "storage: " << _ss << " " << _mmobj->mm_string_cstr(this, _mmobj->mm_function_get_name(this, fun, true), true) << endl;

  // std::cerr << "line mapping: " << " " << _mmobj->mm_string_cstr(_mmobj->mm_function_get_name(fun)) << endl;
  // oop mapping = _mmobj->mm_function_get_line_mapping(fun);
  // std::map<oop, oop>::iterator it = _mmobj->mm_dictionary_begin(mapping);
  // std::map<oop, oop>::iterator end = _mmobj->mm_dictionary_end(mapping);
  // for ( ; it != end; it++) {
  //   std::cerr << untag_small_int(it->first) << " => " << untag_small_int(it->second) << endl;
  // }
  // std::cerr << "loc mapping: " << endl;
  // mapping = _mmobj->mm_function_get_loc_mapping(fun);
  // it = _mmobj->mm_dictionary_begin(mapping);
  // end = _mmobj->mm_dictionary_end(mapping);
  // for ( ; it != end; it++) {
  //   std::cerr << untag_small_int(it->first) << " => [" << _mmobj->mm_list_size(it->second) << "] " ;
  //   for(int i = 0; i < _mmobj->mm_list_size(it->second); i++) {
  //     std::cerr << " " << untag_small_int(_mmobj->mm_list_entry(it->second, i));
  //   }
  //   std::cerr << endl;
  // }

  if (_mmobj->mm_is_context(fun)) {
    restore_fp(_mmobj->mm_context_get_env(this, fun, true), num_params, _mmobj->mm_function_get_env_offset(this, fun, true));
  } else {
    if (_mmobj->mm_function_uses_env(this, fun, true)) {
      setup_fp(num_params, storage_size);
    }
  }

  _cp = fun;
  _mp = _mmobj->mm_function_get_module(this, _cp, true);
  dbg() << "MP for fun load: " << _mp << endl;
  dbg() << "_cp is " << _cp << endl;

  if (_mmobj->mm_function_is_ctor(this, fun, true) and should_allocate) {
    basic_new_and_load(recv);
  }

  if (_mmobj->mm_function_is_prim(this, fun, true)) {
    oop prim_name = _mmobj->mm_function_get_prim_name(this, fun, true);
    std::string str_prim_name = _mmobj->mm_string_cstr(this, prim_name, true);
    int ret = execute_primitive(str_prim_name);
    if (ret == 0) {
      oop value = stack_pop(); //shit
      unload_fun_and_return(value);
      return false;
    } else if (ret == PRIM_RAISED) {
      oop ex_oop = stack_pop(); //shit
      dbg() << "load_fun: prim returned: RAISED " << ex_oop << endl;
      pop_frame();
      maybe_debug_on_raise(ex_oop);
      //we rely on compiler generating a pop instruction to bind ex_oop to the catch var
      stack_push(unwind_with_exception(ex_oop));
        // dbg() << "load_fun: unwind_with_exception reached primitive. c++ throwing...: " << ex_oop << endl;
      dbg() << "load_fun: unwind_with_exception rached catch block for " << ex_oop << endl;
      // oop value = stack_pop(); //shit
      // unload_fun_and_return(value);
      return false;
    }
  }


  // dbg() << "load_fun: module for fun " << fun << " is " << _mp << endl;
  // dbg() << "load_fun: is ctor? " << _mmobj->mm_function_is_ctor(fun) << " alloc? " << should_allocate << endl;
  _ip = _mmobj->mm_function_get_code(this, fun, true);
  // dbg() << "first instruction " << decode_opcode(*_ip) << endl;
  _code_size = _mmobj->mm_function_get_code_size(this, fun);
  maybe_tick_call();
  return true;
}

oop Process::send_0(oop recv, oop selector, int* exc) {
  return do_send(recv, selector, 0, exc);
}

oop Process::send_1(oop recv, oop selector, oop arg, int* exc) {
  stack_push(arg);
  return do_send(recv, selector, 1, exc);
}

oop Process::send(oop recv, oop selector, oop args, int* exc) {
  number num_args = _mmobj->mm_list_size(this, args);
  for (int i = 0; i < num_args; i++) {
    stack_push(_mmobj->mm_list_entry(this, args, i));
  }
  return do_send(recv, selector, num_args, exc);
}

oop Process::do_send(oop recv, oop selector, int num_args, int *exc) {
  dbg() << "-- begin do_send, recv: " << recv
        << ", selector: " << _mmobj->mm_symbol_cstr(this, selector, true) << " #args: " << num_args << endl;

  *exc = 0;
  std::pair<oop, oop> res = lookup(recv, _mmobj->mm_object_vt(recv), selector);

  oop drecv = res.first;
  oop fun = res.second;
  if (!fun) {
    std::stringstream s;
    s << _mmobj->mm_symbol_cstr(this, selector, true) << " not found in object " << recv;
    dbg() << s << endl;
    *exc = 1;
    oop ex = mm_exception("DoesNotUnderstand", s.str().c_str());
    dbg() << "-- end do_send returning exception object " << ex << endl;
    return ex;
  }

  number arity = _mmobj->mm_function_get_num_params(this, fun);
  if (num_args != arity) {
    std::stringstream s;
    s << _mmobj->mm_symbol_cstr(this, selector, true) << ": expects " <<  arity << " but got " << num_args;
    dbg() << s << endl;
    *exc = 1;
    oop ex = mm_exception("ArityError", s.str().c_str());
    dbg() << "-- end do_send returning exception object " << ex << endl;
    return ex;
  }

  try {
    if (load_fun(recv, drecv, fun, true)) {
      fetch_cycle(_bp);
    }
  } catch(mm_rewind e) {
    *exc = 1;
    dbg() << "-- end do_send with excepton: " << e.mm_exception << endl;
    return e.mm_exception;
  }

  oop val = stack_pop();
  dbg() << "-- end do_send, val: " << val << endl;
  return val;
}


// oop Process::do_call_protected(oop fun, int* exc) {
//   int old_state = _state;

//   _state = RUN_STATE;
//   try {
//     oop res = do_call(fun, exc);
//     _state = old_state;
//     return res;
//   } catch( ... ) {
//     _state = old_state;
//     throw;
//   }
// }

oop Process::do_call(oop fun, int* exc) {
  if (!(_mmobj->mm_is_context(fun))) { //since we pass NULL to load_fun
    raise("TypeError", "expecting Context");
  }

  dbg() << "-- begin do_call fun: " << fun << " sp: " << _sp << ", fp: " << _fp << endl;

  *exc = 0;
  try {
    if (load_fun(NULL, NULL, fun, false)) {
      fetch_cycle(_bp);
    }
  } catch(mm_rewind e) {
    *exc = 1;
    dbg() << "-- end do_call with excepton: " << e.mm_exception << endl;
    return e.mm_exception;
  }

  oop val = stack_pop();
  dbg() << "-- end call: " << val << " sp: " << _sp << ", fp: " << _fp << endl;
  return val;
}

oop Process::call(oop fun, oop args, int* exc) {
  if (!(_mmobj->mm_is_context(fun))) { //since we pass NULL to load_fun
    raise("TypeError", "expecting Context");
  }
  number num_args = _mmobj->mm_list_size(this, args);
  number arity = _mmobj->mm_function_get_num_params(this, fun);
  if (num_args != arity) {
    std::stringstream s;
    s << _mmobj->mm_string_cstr(this, _mmobj->mm_function_get_name(this, fun)) << ": expects " <<  arity << " but got " << num_args;
    dbg() << s << endl;
    oop ex = mm_exception("ArityError", s.str().c_str());
    dbg() << "do_call returning exception object " << ex << endl;
    *exc = 1;
    return ex;
  }

  for (int i = 0; i < num_args; i++) {
    stack_push(_mmobj->mm_list_entry(this, args, i));
  }
  return do_call(fun, exc);
}

void Process::fetch_cycle(void* stop_at_bp) {
  dbg() << "begin fetch_cycle fp:" << _fp <<  " stop_fp:" <<  stop_at_bp
        << " ip: " << _ip << endl;

  //at least one instructionn should be executed.  stop_at_bp is usually the
  //top mark of the stack when a function is loaded (_bp). So starting a fetch
  //cycle when this value is smaller than _bp likely means the bytecodes
  //executed more POPs than it should. Since the frame has been compromised,
  //unloading the current function will further mess things up and so on.
  //Thus, let's just do a hard crash if this happens or if _ip -- which
  //we use below -- is 0.

  assert(((_bp >= stop_at_bp) && _ip)); //"base pointer and stop_at_bp are wrong"

  while ((_bp >= stop_at_bp) && _ip) { // && ((_ip - start_ip) * sizeof(bytecode))  < _code_size) {
    // std::cerr << "fp " << _fp << " stop " <<  stop_at_bp << " codesize " <<  _code_size << "ip " << _ip << std::endl;

    bytecode code = *_ip;

    tick();

    // if (_ip != 0) { // the bottommost frame has ip = 0
      _ip++; //this can't be done after dispatch, where an ip for a different fun may be set
             //(thus, doing so would skip the first instruction)
    // }

    int opcode = decode_opcode(code);
    int arg = decode_args(code);
    // if (_state != RUN_STATE) {
    //   char* name = _mmobj->mm_string_cstr(_mmobj->mm_function_get_name(_cp));
    //   std::cerr << " [dispatching] " << name << " " << (_ip-1) << " " << opcode << endl;
    // }
    dispatch(opcode, arg);
    // dbg() << " [end of dispatch] " << opcode << endl;
  }
  dbg() << "end fetch_cycle" << endl;
}

void Process::maybe_tick_return() {
  if (_state == STEP_INTO_STATE) {
    _volatile_breakpoints.push_back(_ip);
  }
  // std::cerr << "maybe_tick_return: " << _step_fp << " " << _fp << endl;
}

void Process::maybe_tick_call() {
  if (_state == STEP_INTO_STATE) {
    _volatile_breakpoints.push_back(_ip);
  }
}

void Process::tick() {
  if (!_ip) return;

  if (_state == RUN_STATE || _state == STEP_INTO_STATE || _state == STEP_OVER_STATE) {
    for(std::list<bytecode*>::iterator it = _volatile_breakpoints.begin(); it != _volatile_breakpoints.end(); it++) {
      // std::cerr << "tick: looking for breakpoint "
      //           << *it << " =?= " << _ip
      //           << " -- opcode: " << decode_opcode(*_ip) << endl;
      if (*it == _ip) {
        // std::cerr << "BREAK on "  << _ip << " " << decode_opcode(*_ip) << endl;
        _volatile_breakpoints.erase(it);
        goto do_pause;
      }
    }
    if (_step_bp >= _bp) {
      // std::cerr << "BREAK on FP "  << _fp << " " << _ip << " " << decode_opcode(*_ip) << endl;
      goto do_pause;
    }
  } else if (_state == STEP_OUT_STATE) {
    if (_step_bp >= _bp) {
      // std::cerr << "BREAK on FP "  << _fp << " " << _ip << " " << decode_opcode(*_ip) << endl;
      goto do_pause;
    }
  } else if (_state == HALT_STATE) {
    // std::cerr << "tick:HALT state. goto pause " << endl;
    goto do_pause;
  }
  return;

  do_pause:
    // std::cerr << "tick: do_pause " << endl;
    int exc;
    oop retval = _dbg_handler.first->send_0(_dbg_handler.second,
                                            _vm->new_symbol("process_paused"), &exc);
    check_and_print_exception(_dbg_handler.first, exc, retval);
    // std::cerr << "tick:HALT "
    //           << _mmobj->mm_string_cstr(_mmobj->mm_function_get_name(_cp))
    //           << " next opcode" <<  decode_opcode(*_ip) << endl;
    _state = HALT_STATE;
    _volatile_breakpoints.clear();
    _step_bp = MM_NULL;
    _control->pause();
    // std::cerr << "Process::tick: resuming..." << endl;
}

void Process::step_into() {
  // std::cerr << "Process::step_into..." << endl;
  bytecode* next = _mmobj->mm_function_next_expr(this, _cp, _ip, true);
  if (next) {
    // std::cerr << "step_into: BP on next op: " << next << " "
    //           << decode_opcode(*next) << endl;
    _volatile_breakpoints.push_back(next);
  }
  _state = STEP_INTO_STATE;
  _control->resume();
}

void Process::step_over() {
  // std::cerr << "Process::step_over resuming ..." << endl;
  bytecode* next = _mmobj->mm_function_next_expr(this, _cp, _ip, true);
  if (next) {
    // std::cerr << "step_over: BP on next op: " << next << " "
    //           << decode_opcode(*next) << endl;
    _volatile_breakpoints.push_back(next);
  }
  _step_bp = *(oop*)_bp; //previous bp
  // std::cerr  << "ste_over will br on: " << _step_bp << " " << std::endl;
  _state = STEP_OVER_STATE;
  _control->resume();
}

void Process::step_over_line() {
  // std::cerr << "Process::step_over_line resuming ..." << endl;
  bytecode* next = _mmobj->mm_function_next_line_expr(this, _cp, _ip, true);
  if (next) {
    // std::cerr << "step_over_line: BP on next op: " << next << " "
    //           << decode_opcode(*next) << endl;
    _volatile_breakpoints.push_back(next);
  }
  _step_bp = *(oop*)_bp; //previous frame
  // std::cerr  << "ste_over_line will br on FP: " << _step_bp << " " << *((oop*)_bp - 6) << std::endl;
  _state = STEP_OVER_STATE;
  _control->resume();
}

void Process::step_out() {
  // std::cerr << "Process::step_out resuming ..." << endl;
  _step_bp = *(oop*)_bp; //previous bp
  // std::cerr  << "ste_out will br on FP: " << _step_bp << " " << std::endl;
  _state = STEP_OUT_STATE;
  _control->resume();
}

void Process::reload_frame() {
  _sp = _bp; //restore sp, ignoring frame data. push_frame/pop_frame are always in sync.
  _ip = _mmobj->mm_function_get_code(this, _cp, true);
}

void Process::handle_send(number num_args) {
  oop selector = stack_pop();
  oop recv = stack_pop(); //(oop) * _sp;

  dbg() << " SEND " << selector << " name: " << _mmobj->mm_symbol_cstr(this, selector)
          << " " << "recv: " << recv << " vt: " << _mmobj->mm_object_vt(recv) << endl;

  std::pair<oop,oop> res = lookup(recv, _mmobj->mm_object_vt(recv), selector);
  oop drecv = res.first;
  oop fun = res.second;
  dbg() << "Lookup FOUND " << fun << endl;

  if (!fun) {
    std::stringstream s;
    s << _mmobj->mm_symbol_cstr(this, selector) << " not found in object " << recv;
    //we rely on compiler generating a pop instruction to bind ex_oop to the catch var
    oop oo_ex = mm_exception("DoesNotUnderstand", s.str().c_str());
    maybe_debug_on_raise(oo_ex);
    stack_push(unwind_with_exception(oo_ex));
    return;
  }

  number arity = _mmobj->mm_function_get_num_params(this, fun);
  if (num_args != arity) {
    std::stringstream s;
    s << _mmobj->mm_string_cstr(this, _mmobj->mm_function_get_name(this, fun)) << ": expects " <<  arity << " but got " << num_args;
    dbg() << s << endl;
    oop ex = mm_exception("ArityError", s.str().c_str());
    dbg() << "handle_send: created exception " << ex << endl;
    maybe_debug_on_raise(ex);
    unwind_with_exception(ex);
    dbg() << "handle_send: unwinded ex. returning exception " << ex << endl;
    stack_push(ex);
    return;
  }
  load_fun(recv, drecv, fun, true);
}

void Process::handle_super_ctor_send(number num_args) {
  oop selector = stack_pop();
  // dbg() << " SUPER: " << selector << " -- " << _mmobj->mm_symbol_cstr(selector) << endl;

  // lookup starts at the parent of rp's class
  oop instance = dp();
  oop klass = _mmobj->mm_object_vt(instance);
  oop pklass = _mmobj->mm_object_delegate(klass);
  oop receiver = _mmobj->mm_object_delegate(instance);

  std::pair<oop, oop> res = lookup(receiver, _mmobj->mm_object_vt(pklass), selector);
  oop drecv = res.first;
  oop fun = res.second;

  if (!fun) {
    dbg() << "super Lookup failed";
    std::stringstream s;
    s << _mmobj->mm_symbol_cstr(this, selector) << " not found in child object " << instance;
    dbg() << s << endl;
    //we rely on compiler generating a pop instruction to bind ex_oop to the catch var
    oop oo_ex = mm_exception("DoesNotUnderstand", s.str().c_str());
    maybe_debug_on_raise(oo_ex);
    stack_push(unwind_with_exception(oo_ex));
    return;
  }

  dbg() << "Lookup FOUND " << fun << endl;


  number arity = _mmobj->mm_function_get_num_params(this, fun);
  if (num_args != arity) {
    std::stringstream s;
    s << "arity and num_args differ: " << num_args << " != " << arity;
    dbg() << s << endl;
    oop oo_ex = mm_exception("ArityError", s.str().c_str());
    maybe_debug_on_raise(oo_ex);
    stack_push(unwind_with_exception(oo_ex));
    return;
  }

  load_fun(rp(), drecv, fun, false);
}

void Process::handle_call(number num_args) {
  // std::cerr << "handle_call" << endl;
  oop fun = stack_pop();
  // std::cerr << "handle_call: fn " << fun << endl;
  number arity = _mmobj->mm_function_get_num_params(this, fun);
  // std::cerr << "handle_call: arity " << arity << endl;
  if (num_args != arity) {
    std::stringstream s;
    s << _mmobj->mm_string_cstr(this, _mmobj->mm_function_get_name(this, fun)) << ": expects " <<  arity << " but got " << num_args;
    dbg() << s << endl;
    oop oo_ex = mm_exception("ArityError", s.str().c_str());
    maybe_debug_on_raise(oo_ex);
    stack_push(unwind_with_exception(oo_ex));
    return;
  }
  load_fun(NULL, NULL, fun, false);

}

void Process::handle_return(oop val) {
  unload_fun_and_return(val);
  maybe_tick_return();
}

void Process::dispatch(int opcode, int arg) {
    // dbg() << " executing " << opcode << " " << arg << endl;
    oop val;
    switch(opcode) {
      case PUSH_LOCAL:
        dbg() << "PUSH_LOCAL " << arg << " " << (oop) *(_fp + arg) << endl;
        stack_push(*(_fp + arg));
        break;
      case PUSH_LITERAL:
        dbg() << "PUSH_LITERAL " << arg << " " << _mmobj->mm_function_get_literal_by_index(this, _cp, arg) << endl;
        stack_push(_mmobj->mm_function_get_literal_by_index(this, _cp, arg));
        break;
      case PUSH_MODULE:
        dbg() << "PUSH_MODULE " << arg << " " << _mp << endl;
        stack_push(_mp);
        break;
      case PUSH_FIELD:
        dbg() << "PUSH_FIELD " << arg << " " << (oop) *(dp() + arg + 2) <<  " dp: " << dp() << endl;
        stack_push(*(dp() + arg + 2));
        break;
      case PUSH_THIS:
        dbg() << "PUSH_THIS " << rp() << endl;
        stack_push(rp());
        break;
      case PUSH_FP:
        dbg() << "PUSH_FP " << arg << " -- " << _fp << endl;
        stack_push(_fp);
        break;

      case PUSH_CONTEXT:
        dbg() << "PUSH_CONTEXT " << arg << endl;
        stack_push(_cp);
        break;
      case PUSH_BIN:
        dbg() << "PUSH_BIN " << arg << endl;
        stack_push(arg);
        break;
      case RETURN_TOP:
        val = stack_pop();
        dbg() << "RETURN_TOP " << arg << " " << val << endl;
        handle_return(val);
        break;
      case RETURN_THIS:
        dbg() << "RETURN_THIS " << rp() << endl;
        handle_return(rp());
        break;
      case POP:
        val =stack_pop();
        dbg() << "POP " << arg << " = " << val << endl;
        break;
      case POP_LOCAL:
        val = stack_pop();
        dbg() << "POP_LOCAL " << arg << " on " << (oop) (_fp + arg) << " -- "
                << (oop) *(_fp + arg) << " = " << val << endl;
        *(_fp + arg) = (word) val;
        break;
      case POP_FIELD:
        val = stack_pop();
        dbg() << "POP_FIELD " << arg << " on " << (oop) (dp() + arg + 2) << " dp: " << dp() << " -- "
                << (oop) *(dp() + arg + 2) << " = " << val << endl; //2: vt, delegate
        *(dp() + arg + 2) = (word) val;
        break;
      case SEND:
        dbg() << "SEND " << arg << endl;
        handle_send(arg);
        break;
      case SUPER_CTOR_SEND:
        handle_super_ctor_send(arg);
        break;
      case CALL:
        dbg() << "CALL " << arg << endl;
        handle_call(arg);
        break;
      case JMP:
        dbg() << "JMP " << arg << " " << endl;
        _ip += (arg -1); //_ip already suffered a ++ in dispatch
        break;
      case JZ:
        val = stack_pop();
        dbg() << "JZ " << arg << " " << val << endl;
        if ((val == MM_FALSE) || (val == MM_NULL)) {
          _ip += (arg -1); //_ip already suffered a ++ in dispatch
        }
        break;
      // case SUPER_SEND:
      //   handle_super_send(arg);
      //   break;
      // case RETURN_THIS:
      //   break;
      default:
        dbg() << "Unknown opcode " << opcode << endl;
        bail("opcode not implemented");
    }
}

std::pair<oop, oop> Process::lookup(oop drecv, oop vt, oop selector) {
  // assert( *(oop*) selector == _core_image->get_prime("Symbol"));
  if (vt == NULL) {
    return std::pair<oop, oop>(MM_NULL, MM_NULL);
  }

  // dbg() << "lookup selector on vt: " << vt << " whose vt is " << _mmobj->mm_object_vt(*(oop*)vt) << endl;

  oop dict = _mmobj->mm_behavior_get_dict(vt);
  dbg() << "Process::lookup dict: " << dict
        << " selector: " << _mmobj->mm_symbol_cstr(this, selector, true) << endl;

  if (_mmobj->mm_dictionary_has_key(this, dict, selector, true)) {
    oop fun = _mmobj->mm_dictionary_get(this, dict, selector, true);
    dbg() << "Lookup of " << selector << " found in " << vt
            << " fun: " << fun << " drecv: " << drecv << endl;
    std::pair<oop,oop> res = std::pair<oop,oop>(drecv, fun);
    return res;
  } else {
    dbg() << "Lookup of " << selector << " NOT found in " << vt << ", recursively looking up..." << endl;
    oop pklass = _mmobj->mm_object_delegate(vt);
    oop delegate = _mmobj->mm_object_delegate(drecv);
    return lookup(delegate, pklass, selector);
  }
}

oop Process::stack_pop() {
  oop val = * (oop*)_sp;
  _sp--;
  dbg() << "     POP " << val << " >> " << _sp << endl;
  return val;
}

void Process::stack_push(oop data) {
  _sp++;
  // dbg() << "     PUSH " << data << " -> " << _sp << endl;
  * (word*) _sp = (word) data;
}

void Process::stack_push(word data) {
  _sp++;
  // dbg() << "     PUSH " << (oop) data << " -> " << _sp << endl;
  * (word*) _sp = data;
}

void Process::stack_push(bytecode* data) {
  _sp++;
  // dbg() << "     PUSH " << data << " -> " << _sp << endl;
  * (word*) _sp = (word) data;
}


int Process::execute_primitive(std::string name) {
  try {
    int val = _vm->get_primitive(this, name)(this);
    dbg() << "primitive " << name << " returned " << val << endl;
    return val;
  } catch(mm_rewind e) {
    dbg() << "primitive " << name << " raised " << e.mm_exception << endl;
    stack_push(e.mm_exception);
    return PRIM_RAISED;
  }
}


bool Process::exception_has_handler(oop e, oop bp) {
  oop cp = cp_from_frame(bp);
  dbg() << "** exception_has_handler e: " << e << " on cp: " << cp << endl;

  if (cp == NULL) {
    return false;
  }

  dbg() << "exception_has_handler: " << _mmobj->mm_string_cstr(this, _mmobj->mm_function_get_name(this, cp, true), true) << endl;

  if (_mmobj->mm_function_is_prim(this, cp, true)) {
    return exception_has_handler(e, *(oop*)bp);
  }

  bytecode* code = _mmobj->mm_function_get_code(this, cp, true);

  number exception_frames_count = _mmobj->mm_function_exception_frames_count(this, cp, true);
  dbg() << "exception frames: " << exception_frames_count << endl;
  if (exception_frames_count == 0) { //cp is unable to handle
    return exception_has_handler(e, *(oop*)bp);
  }

  //exception frames are lexically ordered,
  //so iterating normally we always reach the innermost frame first
  oop exception_frames = _mmobj->mm_function_exception_frames(this, cp, true);
  for(int i = 0; i < exception_frames_count; i++) {
    oop frame_begin = exception_frames + (4 * i);

    word try_block =  *(word*) frame_begin;
    word catch_block = *(word*) (frame_begin + 1);
    oop str_type_oop = *(oop*) (frame_begin + 3);

    oop type_oop;
    if (str_type_oop == MM_NULL) {
      type_oop  = MM_NULL;
    } else {
      dbg() << "fetching exception type for name: " << str_type_oop << endl;
      oop mp = _mmobj->mm_function_get_module(this, cp, true);
      int exc;
      type_oop = send_0(mp, _vm->new_symbol(this, str_type_oop), &exc);
      if (!(exc == 0)) {
        raise("InternalError", "Unable to get exception type from module");
      }
      dbg() << "fetching exception type got " << type_oop << endl;;
    }

    bytecode* ip = ip_from_frame(bp);

    unsigned long instr = ip - code;

    dbg() << "RAISE instr: " << instr << " try: " << try_block
            << " catch: " << catch_block << " type: " << type_oop << endl;

    bool delegates_to = _mmobj->delegates_to(_mmobj->mm_object_vt(e), type_oop);
    dbg() << "delegates_to == " << delegates_to  << endl;

    dbg() << "::" <<  (instr >= try_block) << " " << (instr < catch_block)
            << " " << (type_oop == MM_NULL) << " " << delegates_to << endl;

    if (instr >= try_block && instr < catch_block &&
        (type_oop == MM_NULL || delegates_to)) {
      dbg() << "CAUGHT " << endl;
      return true;
    }
  }

  return exception_has_handler(e, *(oop*)bp);
}

void Process::fail(oop e) {
    int exc;
    oop str = send_0(e, _vm->new_symbol("toString"), &exc);
    assert(exc == 0);
    std::cerr << "Terminated with exception: \""
              << _mmobj->mm_string_cstr(this, str, true) << "\"" << endl;
    bail();
}

oop Process::unwind_with_exception(oop e) {
  dbg() << "** unwind_with_exception e: " << e << " on cp: " << _cp << endl;

  maybe_tick_return();

  if (_cp == NULL) {
    //we are already unwinding exception e.
    //if we can't get Exception.toString to work, then let's just break
    // if (!(exc == 0)) {
    //     raise("InternalError", "Unable to get string representation from exception");
    // }
    fail(e);
  }

  dbg() << "unwind_with_exception: " << _mmobj->mm_string_cstr(this, _mmobj->mm_function_get_name(this, _cp, true), true) << endl;

  if (_mmobj->mm_function_is_prim(this, _cp, true)) {
    dbg() << "->> unwind reached primitive " << _mmobj->mm_string_cstr(this, _mmobj->mm_function_get_prim_name(this, _cp, true), true) << endl;
    throw mm_rewind(e);
  }

  bytecode* code = _mmobj->mm_function_get_code(this, _cp, true);

  number exception_frames_count = _mmobj->mm_function_exception_frames_count(this, _cp, true);
  dbg() << "exception frames: " << exception_frames_count << endl;
  if (exception_frames_count == 0) { //_cp is unable to handle
    pop_frame();
    return unwind_with_exception(e);
  }

  //exception frames are lexically ordered,
  //so iterating normally we always reach the innermost frame first
  oop exception_frames = _mmobj->mm_function_exception_frames(this, _cp, true);
  for(int i = 0; i < exception_frames_count; i++) {
    oop frame_begin = exception_frames + (4 * i);

    word try_block =  *(word*) frame_begin;
    word catch_block = *(word*) (frame_begin + 1);
    oop str_type_oop = *(oop*) (frame_begin + 3);

    oop type_oop;
    if (str_type_oop == MM_NULL) {
      type_oop  = MM_NULL;
    } else {
      dbg() << "fetching exception type for name: " << str_type_oop << endl;
      int exc;
      oop mp = _mmobj->mm_function_get_module(this, _cp, true);
      type_oop = send_0(mp, _vm->new_symbol(this, str_type_oop), &exc);
      if (!(exc == 0)) {
        raise("InternalError", "Unable to get exception type from module");
      }
      dbg() << "fetching exception type got " << type_oop << endl;;
    }

    word instr = _ip - code;

    dbg() << "RAISE instr: " << instr << " try: " << try_block
            << " catch: " << catch_block << " type: " << type_oop << endl;

    bool delegates_to = _mmobj->delegates_to(_mmobj->mm_object_vt(e), type_oop);
    dbg() << "delegates_to == " << delegates_to  << endl;
    if (instr >= try_block && instr < catch_block &&
        (type_oop == MM_NULL || delegates_to)) {
      dbg() << "CAUGHT " << endl;
      _ip = code + catch_block;
      return e;
    }
  }

  pop_frame();
  return unwind_with_exception(e);
}


void Process::raise(const char* ex_type_name, const char* msg) {
  dbg() << "Process::raise " << ex_type_name << " -- " << msg << endl;
  //TODO: this is used by mmc_image. I think we should just return the
  // exception and let that class deal with it.
  throw mm_rewind(mm_exception(ex_type_name, msg));
}

oop Process::mm_exception(const char* ex_type_name, const char* msg) {
  dbg() << "mm_exception: " << ex_type_name << " -- " << msg << endl;
  oop ex_type = _vm->get_prime(ex_type_name);

  int exc;
  oop exobj = send_1(ex_type, _vm->new_symbol("new"), _mmobj->mm_string_new(msg), &exc);
  //if this fails, can't call raise() because it calls mm_exception again
  //recursively. So let's just bail.
  assert(exc == 0);
  dbg() << "mm_exception: returning ex: " << exobj << endl;
  return exobj;
}

void Process::halt_and_debug() {
  // std::cerr << "Process::halt_and_debug" << endl;
  _dbg_handler = _vm->start_debugger(this);
  pause();
}

void Process::maybe_debug_on_raise(oop ex_oop) {
  if (_vm->running_online() && !exception_has_handler(ex_oop, _bp)) {
    halt_and_debug();
  }
}

unsigned int Process::stack_depth() {
  return  _stack_depth;
}

oop Process::bp_at(unsigned int idx) { //backwards 0 is current
  if (idx == 0) {
    // std::cerr << "Process::bp_at BP[" << idx << "] ret&: " << &_bp << endl;
    return (oop) &_bp;
  }
  oop bp = _bp;
  for (unsigned int i = 0; i < idx-1; i++) {
    bp = *(oop*) bp;
    // std::cerr << "Process::bp_at " << i << " = " << bp << endl;
  }
  // std::cerr << "Process::bp_at BP[" << idx << "] ret: " << bp << endl;
  return bp;
}

// void Process::eval_in_frame(const char* text, number frame_idx) {
//   // oop bp = bt_at(frame_idx);
//   // oop cp = *((oop*)bp - 5);
//   // oop mp = _mmobj->mm_function_get_module(cp);
//   // oop cmod = _mmobj->mm_module_get_cmod(mp);

//   // // get the env table from cp. It should have all names / offsets. See it the order of the names
//   // // matches their _ep offsets, If not, build `vars` from the table.
//   // std::list<std::string> lst = proc->mmobj()->mm_list_to_cstring_list();

//   // int exc;
//   // oop cfun = _vm->compile_fun(text, , cmod, &exc);

//   // if (exc != 0) {
//   //   proc->stack_push(cfun);
//   //   return PRIM_RAISED;
//   // }
// }


std::ostream& Process::dbg() {
  if (!_is_dbg) {
    debug() << "PROC[target]";
  } else {
    debug() << "PROC[debugger]";
  }
  return debug();
}
