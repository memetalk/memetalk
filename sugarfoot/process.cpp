#include <stdlib.h>
#include <string>
#include <stdlib.h>
#include <assert.h>
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

Process::Process(VM* vm)
  : _vm(vm), _mmobj(vm->mmobj()), _control(new ProcessControl()) {
  init();
  _state = RUNNING_STATE;
}

oop Process::run(oop recv, oop selector_sym) {
  //atm, we don't need to check exc
  //since ::unwind_with_exception will terminate the vm
  //if there is no more stack to unwind.
  int exc;
  return send_0(recv, selector_sym, &exc);
}

void Process::init() {
  _stack = (word*) malloc(DEFAULT_STACK_SIZE);
  // debug() << " Initial stack " << _stack << endl;

  _state = INVALID_STATE;
  _sp = _stack; //stack
  _fp = _stack; //frame
  _bp = NULL; //base
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
  debug() << "push_frame: fp: " << _fp << endl;

  _fp = _sp;
  for (int i = 0; i < num_locals; i++) {
    stack_push((oop)0);
  }

  debug() << "push_frame arity: " << arity << " num locals: " << num_locals
          << " fp: " << (oop) fp << " cp: " << _cp << " ip: " << _ip << " ep: " << _ep
          << " rp: " << _rp << " dp: " << _dp << " mp: " << _mp << endl;

  stack_push(fp);
  stack_push(_cp);
  stack_push(_ip);
  stack_push(_ep);
  stack_push(_rp);
  stack_push(_dp);
  stack_push(_bp);
  _bp = _sp;
  // debug() << " push frame SP is: " << _sp << endl;
}

void Process::pop_frame() {
  number arity = *_fp;
  word fp = (word) _fp;

  // debug() << "pop_frame begin SP: " << _sp << endl;

  _sp = _bp; //restore sp, ignoring frame data. push_frame/pop_frame are always in sync.

  _bp = stack_pop();
  _dp = stack_pop();
  _rp = stack_pop();
  _ep = stack_pop();
  _ip = (bytecode*) stack_pop();
  _cp = stack_pop();
  _fp = stack_pop();

  debug() << "pop_frame: fp: " << _fp << " rp: " << _rp << " dp: " << _dp << endl;

  // debug() << "pop_frame [fm:" << --frame_count << " arity: " << arity
  //         << " fp: " << _fp << " cp: " << _cp << " ip: " << _ip << " ep: " << _ep
  //         << " rp: " << _rp << " dp: " << _dp << " mp (before): " << _mp << endl;

  if (_cp) {// first frame has _cp = null
    // debug() << "pop_frame: getting module from fun " << _cp << endl;
    _mp = _mmobj->mm_function_get_module(_cp);
    debug() << "MP for fun unloaded: " << _mp << endl;
  } else {
    debug() << "++ MP for fun unloaded is null!" << endl;
  }

  _sp = (oop) fp - (arity + 1);
  // debug() << "-- pop frame SP:" <<  _sp << endl;
}

void Process::unload_fun_and_return(oop retval) {
  pop_frame();
  stack_push(retval);
}

oop Process::ctor_rdp_for(oop rp, oop cp) {
  debug() << "ctor_rdp_for rp: " << rp << ", cp: " << cp << endl;
  if (!rp) {
    bail("no rdp for ctor!");
  }
  oop vt = _mmobj->mm_object_vt(rp);
  oop cclass = _mmobj->mm_class_get_compiled_class(vt);

  oop other_cclass = _mmobj->mm_function_get_owner(cp);

  debug() << "rdp_for_ctor cclass: " << cclass << ", other cclass: " << other_cclass << endl;

  if (cclass == other_cclass) {
    return rp;
  } else {
    return ctor_rdp_for(_mmobj->mm_object_delegate(rp), cp);
  }
}

void Process::basic_new_and_load(oop klass) {
  oop instance = _mmobj->alloc_instance(klass);
  _rp = instance;
  _dp = ctor_rdp_for(instance, _cp);
  debug() << "basic_new: " << instance << " dp: " << _dp << endl;
  if (_ep) {
    ((oop*)_ep)[0] = _rp;
    ((oop*)_ep)[1] = _dp;
  }
}

void Process::setup_ep(oop fun, oop recv, oop drecv) {
  number params = _mmobj->mm_function_get_num_params(fun);
  number size = _mmobj->mm_function_get_num_locals_or_env(fun);
  _ep = (oop) calloc(sizeof(oop), size + 2); //+2: space for rp, dp
  debug() << "Allocated ep: " << _ep << " - " << " size: " << size
          << " params: " << params
          << " rp/dp: "<< recv << " " << drecv << endl;

  ((oop*)_ep)[0] = recv;
  ((oop*)_ep)[1] = drecv;

  copy_params_to_env(params, _mmobj->mm_function_get_env_offset(fun));
}

void Process::copy_params_to_env(number params, number env_offset) {
  debug() << "Process::copy_params_to_env " << params << " " << env_offset << endl;

  for (int i = 0; i < params; i++) {
    debug() << "ep[" << i+2+env_offset << "] = " << * (oop*)(_fp - (i+1)) << endl;
    ((oop*)_ep)[i+2+env_offset] = * (oop*)(_fp - (i+1));
  }
}

bool Process::load_fun(oop recv, oop drecv, oop fun, bool should_allocate) {
  assert(_mmobj->mm_is_function(fun) || _mmobj->mm_is_context(fun));

  if (_mmobj->mm_function_is_getter(fun)) {
    number idx = _mmobj->mm_function_access_field(fun);
    debug() << "GETTER: idx " << idx << " on " << recv << endl;
    oop val = ((oop*)drecv)[idx];
    debug() << "GETTER: pushing retval: " << val << endl;
    stack_push(val);
    return false;
  }


  push_frame(_mmobj->mm_function_get_num_params(fun),
             _mmobj->mm_function_get_num_locals_or_env(fun));

  std::cerr << "line mapping: " << endl;
  oop mapping = _mmobj->mm_function_get_line_mapping(fun);
  std::map<oop, oop>::iterator it = _mmobj->mm_dictionary_begin(mapping);
  std::map<oop, oop>::iterator end = _mmobj->mm_dictionary_end(mapping);
  for ( ; it != end; it++) {
    std::cerr << untag_small_int(it->first) << " => " << untag_small_int(it->second) << endl;
  }
  std::cerr << "loc mapping: " << endl;
  mapping = _mmobj->mm_function_get_loc_mapping(fun);
  it = _mmobj->mm_dictionary_begin(mapping);
  end = _mmobj->mm_dictionary_end(mapping);
  for ( ; it != end; it++) {
    std::cerr << untag_small_int(it->first) << " => [" << _mmobj->mm_list_size(it->second) << "] " ;
    for(int i = 0; i < _mmobj->mm_list_size(it->second); i++) {
      std::cerr << " " << untag_small_int(_mmobj->mm_list_entry(it->second, i));
    }
    std::cerr << endl;
  }

  if (_mmobj->mm_is_context(fun)) {
    _ep = _mmobj->mm_context_get_env(fun);
    debug() << "setting up ctx ep: " << _ep << endl;
    _rp = ((oop*)_ep)[0];
    _dp = ((oop*)_ep)[1];
    debug() << "loaded ep with rp/dp: " << _rp << " " << _dp << endl;
    copy_params_to_env(_mmobj->mm_function_get_num_params(fun),
                       _mmobj->mm_function_get_env_offset(fun));
  } else {
    if (_mmobj->mm_function_uses_env(fun)) {
      setup_ep(fun, recv, drecv);
      _rp = NULL; //to remember me to use ep[rp] and dp[rp] if ep exists when executing bytecodes
      _dp = NULL;
      debug() << "look out! we should use ep[dp] and ep[rp]: " << _rp << " " << _dp << endl;
    } else {
      _ep = NULL;
      _rp = recv;
      _dp = drecv;
    }
  }

  _cp = fun;
  _mp = _mmobj->mm_function_get_module(_cp);
  debug() << "MP for fun load: " << _mp << endl;

  if (_mmobj->mm_function_is_ctor(fun) and should_allocate) {
    basic_new_and_load(recv);
  }

  if (_mmobj->mm_function_is_prim(fun)) {
    oop prim_name = _mmobj->mm_function_get_prim_name(fun);
    std::string str_prim_name = _mmobj->mm_string_cstr(prim_name);
    int ret = execute_primitive(str_prim_name);
    if (ret == 0) {
      oop value = stack_pop(); //shit
      unload_fun_and_return(value);
      return false;
    } else if (ret == PRIM_RAISED) {
      oop ex_oop = stack_pop(); //shit
      debug() << "load_fun: prim returned: RAISED " << ex_oop << endl;
      pop_frame();
      if (_vm->running_online() && !exception_has_handler(ex_oop, _bp)) {
        _vm->start_debugger(this);
        pause();
      }
      //we rely on compiler generating a pop instruction to bind ex_oop to the catch var
      stack_push(unwind_with_exception(ex_oop));
        // debug() << "load_fun: unwind_with_exception reached primitive. c++ throwing...: " << ex_oop << endl;
      debug() << "load_fun: unwind_with_exception rached catch block for " << ex_oop << endl;
      // oop value = stack_pop(); //shit
      // unload_fun_and_return(value);
      return false;
    }
  }


  // debug() << "load_fun: module for fun " << fun << " is " << _mp << endl;
  // debug() << "load_fun: is ctor? " << _mmobj->mm_function_is_ctor(fun) << " alloc? " << should_allocate << endl;
  _ip = _mmobj->mm_function_get_code(fun);
  // debug() << "first instruction " << decode_opcode(*_ip) << endl;
  _code_size = _mmobj->mm_function_get_code_size(fun);
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
  number num_args = _mmobj->mm_list_size(args);
  for (int i = 0; i < num_args; i++) {
    stack_push(_mmobj->mm_list_entry(args, i));
  }
  return do_send(recv, selector, num_args, exc);
}

oop Process::do_send(oop recv, oop selector, int num_args, int *exc) {
  debug() << "-- begin do_send, recv: " << recv
          << ", selector: " << _mmobj->mm_symbol_cstr(selector) << " #args: " << num_args << endl;

  *exc = 0;
  std::pair<oop, oop> res = lookup(recv, _mmobj->mm_object_vt(recv), selector);

  oop drecv = res.first;
  oop fun = res.second;
  if (!fun) {
    std::stringstream s;
    s << _mmobj->mm_symbol_cstr(selector) << " not found in object " << recv;
    debug() << s << endl;
    *exc = 1;
    oop ex = mm_exception("DoesNotUnderstand", s.str().c_str());
    debug() << "-- end do_send returning exception object " << ex << endl;
    return ex;
  }

  number arity = _mmobj->mm_function_get_num_params(fun);
  if (num_args != arity) {
    std::stringstream s;
    s << _mmobj->mm_symbol_cstr(selector) << ": expects " <<  arity << " but got " << num_args;
    debug() << s << endl;
    *exc = 1;
    oop ex = mm_exception("ArityError", s.str().c_str());
    debug() << "-- end do_send returning exception object " << ex << endl;
    return ex;
  }

  oop stop_at_fp = _fp;
  try {
    if (load_fun(recv, drecv, fun, true)) {
      fetch_cycle(stop_at_fp);
    }
  } catch(mm_rewind e) {
    *exc = 1;
    debug() << "-- end do_send with excepton: " << e.mm_exception << endl;
    return e.mm_exception;
  }

  oop val = stack_pop();
  debug() << "-- end do_send, val: " << val << endl;
  return val;
}



oop Process::do_call(oop fun, int* exc) {
  assert(_mmobj->mm_is_context(fun)); //since we pass NULL to load_fun

  debug() << "-- begin do_call, sp: " << _sp << ", fp: " << _fp << endl;
  oop stop_at_fp = _fp;

  *exc = 0;
  try {
    if (load_fun(NULL, NULL, fun, false)) {
      fetch_cycle(stop_at_fp);
    }
  } catch(mm_rewind e) {
    *exc = 1;
    debug() << "-- end do_call with excepton: " << e.mm_exception << endl;
    return e.mm_exception;
  }

  oop val = stack_pop();
  debug() << "-- end call: " << val << " sp: " << _sp << ", fp: " << _fp << endl;
  return val;
}

oop Process::do_call(oop fun, oop args, int* exc) {
  assert(_mmobj->mm_is_context(fun)); //since we pass NULL to load_fun

  number num_args = _mmobj->mm_list_size(args);
  number arity = _mmobj->mm_function_get_num_params(fun);
  if (num_args != arity) {
    std::stringstream s;
    s << _mmobj->mm_string_cstr(_mmobj->mm_function_get_name(fun)) << ": expects " <<  arity << " but got " << num_args;
    debug() << s << endl;
    oop ex = mm_exception("ArityError", s.str().c_str());
    debug() << "do_call returning exception object " << ex << endl;
    *exc = 1;
    return ex;
  }

  for (int i = 0; i < num_args; i++) {
    stack_push(_mmobj->mm_list_entry(args, i));
  }
  return do_call(fun, exc);
}

void Process::fetch_cycle(void* stop_at_fp) {
  debug() << "begin fetch_cycle fp:" << _fp <<  " stop_fp:" <<  stop_at_fp
          << " ip: " << _ip << endl;
  while ((_fp > stop_at_fp) && _ip) { // && ((_ip - start_ip) * sizeof(bytecode))  < _code_size) {
    // debug() << "fp " << _fp << " stop " <<  stop_at_fp << " ip-start " <<  (_ip - start_ip)  << " codesize " <<  _code_size <<
    //   "ip " << _ip << std::endl;

    tick();

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
  debug() << "end fetch_cycle" << endl;
}

void Process::tick() {
  switch(_state) {
    case PAUSED_STATE:
//      debug() << "process::tick: paused" << endl;
      _control->pause();
      break;
    case RUNNING_STATE:
//      debug() << "process::tick: resuming" << endl;
      _control->resume();
      break;
  }
}

void Process::step() {
  debug() << "Process::step resuming ..." << endl;
  _control->resume();
}

void Process::handle_send(number num_args) {
  oop selector = stack_pop();
  oop recv = stack_pop(); //(oop) * _sp;

  debug() << " SEND " << selector << " name: " << _mmobj->mm_symbol_cstr(selector)
          << " " << "recv: " << recv << endl;

  std::pair<oop,oop> res = lookup(recv, _mmobj->mm_object_vt(recv), selector);
  oop drecv = res.first;
  oop fun = res.second;
  debug() << "Lookup FOUND " << fun << endl;

  if (!fun) {
    std::stringstream s;
    s << _mmobj->mm_symbol_cstr(selector) << " not found in object " << recv;
    //we rely on compiler generating a pop instruction to bind ex_oop to the catch var
    stack_push(unwind_with_exception(mm_exception("DoesNotUnderstand", s.str().c_str())));
    return;
  }

  number arity = _mmobj->mm_function_get_num_params(fun);
  if (num_args != arity) {
    std::stringstream s;
    s << _mmobj->mm_string_cstr(_mmobj->mm_function_get_name(fun)) << ": expects " <<  arity << " but got " << num_args;
    debug() << s << endl;
    oop ex = mm_exception("ArityError", s.str().c_str());
    debug() << "handle_send: created exception " << ex << endl;
    unwind_with_exception(ex);
    debug() << "handle_send: unwinded ex. returning exception " << ex << endl;
    stack_push(ex);
    return;
  }
  load_fun(recv, drecv, fun, true);
}

void Process::handle_super_ctor_send(number num_args) {
  oop selector = stack_pop();
  // debug() << " SUPER: " << selector << " -- " << _mmobj->mm_symbol_cstr(selector) << endl;

  // lookup starts at the parent of rp's class
  oop instance = get_dp();
  oop klass = _mmobj->mm_object_vt(instance);
  oop pklass = _mmobj->mm_object_delegate(klass);
  oop receiver = _mmobj->mm_object_delegate(instance);

  std::pair<oop, oop> res = lookup(receiver, _mmobj->mm_object_vt(pklass), selector);
  oop drecv = res.first;
  oop fun = res.second;

  if (!fun) {
    debug() << "super Lookup failed";
    std::stringstream s;
    s << _mmobj->mm_symbol_cstr(selector) << " not found in child object " << instance;
    debug() << s << endl;
    //we rely on compiler generating a pop instruction to bind ex_oop to the catch var
    stack_push(unwind_with_exception(mm_exception("DoesNotUnderstand", s.str().c_str())));
    return;
  }

  debug() << "Lookup FOUND " << fun << endl;


  number arity = _mmobj->mm_function_get_num_params(fun);
  if (num_args != arity) {
    std::stringstream s;
    s << "arity and num_args differ: " << num_args << " != " << arity;
    debug() << s << endl;
    stack_push(unwind_with_exception(mm_exception("DoesNotUnderstand", s.str().c_str())));
    return;
  }

  load_fun(get_rp(), drecv, fun, false);
}

void Process::handle_call(number num_args) {
  oop fun = stack_pop();
  number arity = _mmobj->mm_function_get_num_params(fun);
  if (num_args != arity) {
    std::stringstream s;
    s << _mmobj->mm_string_cstr(_mmobj->mm_function_get_name(fun)) << ": expects " <<  arity << " but got " << num_args;
    debug() << s << endl;
    stack_push(unwind_with_exception(mm_exception("ArityError", s.str().c_str())));
    return;
  }
  load_fun(NULL, NULL, fun, false);

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
        debug() << "PUSH_MODULE " << arg << " " << _mp << endl;
        stack_push(_mp);
        break;
      case PUSH_FIELD:
        debug() << "PUSH_FIELD " << arg << " " << (oop) *(get_dp() + arg + 2) <<  " dp: " << get_dp() << endl;
        stack_push(*(get_dp() + arg + 2));
        break;
      case PUSH_THIS:
        if (_ep == NULL) {
          debug() << "PUSH_THIS " << arg << " " << get_rp() << endl;
          stack_push(get_rp());
        } else {
          debug() << "PUSH_THIS [env] " << arg << " " << ((oop*)_ep)[0] << endl;
          stack_push(((oop*)_ep)[0]);
        }
        break;
      case PUSH_ENV:
        debug() << "PUSH_ENV " << arg << " -- " << _ep << " " << (oop*)_ep[arg+2] << endl;//+2: skip dp,rp
        stack_push(((oop*)_ep)[arg+2]);
        break;
      case PUSH_EP:
        debug() << "PUSH_EP " << arg << " -- " << _ep << endl;
        stack_push(_ep);
        break;

      case PUSH_CONTEXT:
        debug() << "PUSH_CONTEXT " << arg << endl;
        stack_push(_cp);
        break;
      case PUSH_BIN:
        debug() << "PUSH_BIN " << arg << endl;
        stack_push(arg);
        break;
      case RETURN_TOP:
        val = stack_pop();
        debug() << "RETURN_TOP " << arg << " " << val << endl;
        unload_fun_and_return(val);
        break;
      case RETURN_THIS:
        if (_ep == NULL) {
          debug() << "RETURN_THIS " << get_rp() << endl;
          unload_fun_and_return(get_rp());
        } else {
          debug() << "RETURN_THIS [env] " << arg << " " << ((oop*)_ep)[0] << endl;
          unload_fun_and_return(((oop*)_ep)[0]);
        }
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
        debug() << "POP_FIELD " << arg << " on " << (oop) (get_dp() + arg + 2) << " dp: " << get_dp() << " -- "
                << (oop) *(get_dp() + arg + 2) << " = " << val << endl; //2: vt, delegate
        *(get_dp() + arg + 2) = (word) val;
        break;
      case POP_ENV:
        val = stack_pop();
        debug() << "POP_ENV " << arg << " ep: " << _ep << " "
                << (oop) *(_ep + arg + 2) << " = " << val << endl; //+2: rp, dp
        *(_ep + arg + 2) = (word) val;
        break;
      case SEND:
        debug() << "SEND " << arg << endl;
        handle_send(arg);
        break;
      case SUPER_CTOR_SEND:
        handle_super_ctor_send(arg);
        break;
      case CALL:
        debug() << "CALL " << arg << endl;
        handle_call(arg);
        break;
      case JMP:
        debug() << "JMP " << arg << " " << endl;
        _ip += (arg -1); //_ip already suffered a ++ in dispatch
        break;
      case JZ:
        val = stack_pop();
        debug() << "JZ " << arg << " " << val << endl;
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
        debug() << "Unknown opcode " << opcode << endl;
        bail("opcode not implemented");
    }
}

std::pair<oop, oop> Process::lookup(oop drecv, oop vt, oop selector) {
  // assert( *(oop*) selector == _core_image->get_prime("Symbol"));
  if (vt == NULL) {
    return std::pair<oop, oop>(MM_NULL, MM_NULL);
  }

  // debug() << "lookup selector on vt: " << vt << " whose vt is " << _mmobj->mm_object_vt(*(oop*)vt) << endl;

  oop dict = _mmobj->mm_behavior_get_dict(vt);
  debug() << "Process::lookup dict: " << dict
          << " selector: " << _mmobj->mm_symbol_cstr(selector) << endl;

  if (_mmobj->mm_dictionary_has_key(dict, selector)) {
    oop fun = _mmobj->mm_dictionary_get(dict, selector);
    debug() << "Lookup of " << selector << " found in " << vt
            << " fun: " << fun << " drecv: " << drecv << endl;
    std::pair<oop,oop> res = std::pair<oop,oop>(drecv, fun);
    return res;
  } else {
    debug() << "Lookup of " << selector << " NOT found in " << vt << ", recursively looking up..." << endl;
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


int Process::execute_primitive(std::string name) {
  int val = _vm->get_primitive(name)(this);
  debug() << "primitive " << name << " returned " << val << endl;
  return val;
}


bool Process::exception_has_handler(oop e, oop bp) {
  oop cp = * ((oop*)bp - 5);
  debug() << "** exception_has_handler e: " << e << " on cp: " << cp << endl;

  if (cp == NULL) {
    return false;
  }

  debug() << "exception_has_handler: " << _mmobj->mm_string_cstr(_mmobj->mm_function_get_name(cp)) << endl;

  if (_mmobj->mm_function_is_prim(cp)) {
    return exception_has_handler(e, *(oop*)bp);
  }

  bytecode* code = _mmobj->mm_function_get_code(cp);

  number exception_frames_count = _mmobj->mm_function_exception_frames_count(cp);
  debug() << "exception frames: " << exception_frames_count << endl;
  if (exception_frames_count == 0) { //cp is unable to handle
    return exception_has_handler(e, *(oop*)bp);
  }

  //exception frames are lexically ordered,
  //so iterating normally we always reach the innermost frame first
  oop exception_frames = _mmobj->mm_function_exception_frames(cp);
  for(int i = 0; i < exception_frames_count; i++) {
    oop frame_begin = exception_frames + (4 * i);

    word try_block =  *(word*) frame_begin;
    word catch_block = *(word*) (frame_begin + 1);
    oop str_type_oop = *(oop*) (frame_begin + 3);

    oop type_oop;
    if (str_type_oop == MM_NULL) {
      type_oop  = MM_NULL;
    } else {
      debug() << "fetching exception type for name: " << str_type_oop << endl;
      oop mp = _mmobj->mm_function_get_module(cp);
      int exc;
      type_oop = send_0(mp, _vm->new_symbol(str_type_oop), &exc);
      assert(exc == 0);
      debug() << "fetching exception type got " << type_oop << endl;;
    }

    bytecode* ip = (bytecode*) * ((oop*)bp - 4);

    unsigned long instr = ip - code;

    debug() << "RAISE instr: " << instr << " try: " << try_block
            << " catch: " << catch_block << " type: " << type_oop << endl;

    bool delegates_to = _mmobj->delegates_to(_mmobj->mm_object_vt(e), type_oop);
    debug() << "delegates_to == " << delegates_to  << endl;

    debug() << "::" <<  (instr >= try_block) << " " << (instr < catch_block)
            << " " << (type_oop == MM_NULL) << " " << delegates_to << endl;

    if (instr >= try_block && instr < catch_block &&
        (type_oop == MM_NULL || delegates_to)) {
      debug() << "CAUGHT " << endl;
      return true;
    }
  }

  return exception_has_handler(e, *(oop*)bp);
}

oop Process::unwind_with_exception(oop e) {
  debug() << "** unwind_with_exception e: " << e << " on cp: " << _cp << endl;

  tick();

  if (_cp == NULL) {
    int exc;
    oop str = send_0(e, _vm->new_symbol("toString"), &exc);
    assert(exc == 0);
    std::cerr << "Terminated with exception: \""
              << _mmobj->mm_string_cstr(str) << "\"" << endl;
    bail();
  }

  debug() << "unwind_with_exception: " << _mmobj->mm_string_cstr(_mmobj->mm_function_get_name(_cp)) << endl;

  if (_mmobj->mm_function_is_prim(_cp)) {
    debug() << "->> unwind reached primitive " << _mmobj->mm_string_cstr(_mmobj->mm_function_get_prim_name(_cp)) << endl;
    throw mm_rewind(e);
  }

  bytecode* code = _mmobj->mm_function_get_code(_cp);

  number exception_frames_count = _mmobj->mm_function_exception_frames_count(_cp);
  debug() << "exception frames: " << exception_frames_count << endl;
  if (exception_frames_count == 0) { //_cp is unable to handle
    pop_frame();
    return unwind_with_exception(e);
  }

  //exception frames are lexically ordered,
  //so iterating normally we always reach the innermost frame first
  oop exception_frames = _mmobj->mm_function_exception_frames(_cp);
  for(int i = 0; i < exception_frames_count; i++) {
    oop frame_begin = exception_frames + (4 * i);

    word try_block =  *(word*) frame_begin;
    word catch_block = *(word*) (frame_begin + 1);
    oop str_type_oop = *(oop*) (frame_begin + 3);

    oop type_oop;
    if (str_type_oop == MM_NULL) {
      type_oop  = MM_NULL;
    } else {
      debug() << "fetching exception type for name: " << str_type_oop << endl;
      int exc;
      oop mp = _mmobj->mm_function_get_module(_cp);
      type_oop = send_0(mp, _vm->new_symbol(str_type_oop), &exc);
      assert(exc == 0);
      debug() << "fetching exception type got " << type_oop << endl;;
    }

    word instr = _ip - code;

    debug() << "RAISE instr: " << instr << " try: " << try_block
            << " catch: " << catch_block << " type: " << type_oop << endl;

    bool delegates_to = _mmobj->delegates_to(_mmobj->mm_object_vt(e), type_oop);
    debug() << "delegates_to == " << delegates_to  << endl;
    if (instr >= try_block && instr < catch_block &&
        (type_oop == MM_NULL || delegates_to)) {
      debug() << "CAUGHT " << endl;
      _ip = code + catch_block;
      return e;
    }
  }

  pop_frame();
  return unwind_with_exception(e);
}


void Process::raise(const char* ex_type_name, const char* msg) {
  debug() << "Process::raise" << ex_type_name << " -- " << msg << endl;
  //TODO: this is used by mmc_image. I think we should just return the
  // exception and let that class deal with it.
  throw mm_rewind(mm_exception(ex_type_name, msg));
}

oop Process::mm_exception(const char* ex_type_name, const char* msg) {
  debug() << "mm_exception: " << ex_type_name << " -- " << msg << endl;
  oop ex_type = _vm->get_prime(ex_type_name);

  int exc;
  oop exobj = send_1(ex_type, _vm->new_symbol("new"), _mmobj->mm_string_new(msg), &exc);
  assert(exc == 0);
  debug() << "mm_exception: returning ex: " << exobj << endl;
  return exobj;
}
