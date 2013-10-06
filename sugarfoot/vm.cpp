#include "vm.hpp"
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <string.h>

static std::ostream& debug() {
  return std::cout;
}

static void bail(const char* msg) {
  std::cout << msg << "\n";
  exit(1);
}

// ------------------- Image -------------------


Image::Image(VM* vm, std::string data) {
  _imod = vm->heap_store(data, data.size());

  // for each item in data table:
  //  vm->heap_store(entry, entry_size)
  // lookup entry fun in table:
  //  _entry = vm->heap_store(ptr, size of FN object)
}



// ------------------- VMProcess -------------------

VMProcess::VMProcess(VM* vm)
  : _vm(vm),
    _state(STATE_RUNNING),
    _call_stack(0),
    _mp(0),
    _cp(0),
    _rp(0),
    _rdp(0),
    _ep(0),
    _ip(0)
{
  _call_stack = (call_stack_t*) malloc(sizeof(call_stack_t)*STACK_INITIAL_SIZE);
}

void VMProcess::push_activation_record() {
}

void VMProcess::pop_activation_record() {
}

void VMProcess::init_activation_record(oop recv, oop drecv, oop fn, oop args) {
  _cp = fn;
  _ep = NULL;
  _mp = MM_FUN_MODULE(fn);

  //add args to the callstack or _env

        // self.locals({})
        // if not id_eq(self.interpreter.get_vt(method), self.interpreter.core_imod['Context']):
        //     self.reg('r_rp', recv)
        //     self.reg('r_rdp', drecv)
        //     if not method["compiled_function"]["uses_env"] and \
        //             id_eq(self.interpreter.get_vt(method), self.interpreter.core_imod['Function']):
        //         # normal fun, put args in the stack
        //         if self.has_vararg(method["compiled_function"]["params"]):
        //             regular = method["compiled_function"]["params"][0:-1]
        //             reg_len = len(regular)
        //             for k,v in zip(regular,args):
        //                 self.locals()[k] = v
        //             va_name = method["compiled_function"]["params"][-1][1]
        //             self.locals()[va_name] = args[reg_len:]
        //         else:
        //             for k,v in zip(method["compiled_function"]["params"],args):
        //                 self.locals()[k] = v
        //     # normal fun using env, initialize one
        //     elif method["compiled_function"]["uses_env"] and \
        //             id_eq(self.interpreter.get_vt(method), self.interpreter.core_imod['Function']):
        //         self.reg('r_ep', dict(method["compiled_function"]['env_table_skel'].items()))
        //         self.reg('r_ep')["r_rp"] = self.reg('r_rp')
        //         self.reg('r_ep')["r_rdp"] = self.reg('r_rdp') # usually receivers are on stack.
        //                                         # I need them here: when calling a
        //                                         # closure that references a 'this'
        //         # put args in the env
        //         for k,v in zip(method["compiled_function"]["params"],args):
        //             self.env_set_value(k,v) # cp should be set already
        // else:
        //     self.reg('r_ep', method["env"])
        //     if 'r_rp' in self.reg('r_ep'):
        //         self.reg('r_rp', self.reg('r_ep')['r_rp'])
        //     if 'r_rdp' in self.reg('r_ep'):
        //         self.reg('r_rdp', self.reg('r_ep')['r_rdp'])
        //     # put args in the env
        //     for k,v in zip(method["compiled_function"]["params"],args):
        //         self.env_set_value(k, v)
}

int VMProcess::run_image(std::string data) {
  Image img(_vm, data);

  _rp = _rdp = _mp = img.oop_imodule();
  oop recv = img.oop_imodule();
  oop res = send(recv, recv, img.oop_entry());
  return (int) res;
}

oop VMProcess::send(oop recv, oop drecv, oop fn) {
  push_activation_record();
  init_activation_record(recv, drecv, fn, NULL);

        // self.setup_parameters_and_registers(recv, drecv, fun, args)
        // if should_allocate and fun["compiled_function"]['is_ctor']:
        //     #allocate new instance and make it the receiver
        //     self.reg('r_rp', self.interpreter.create_instance(self.reg('r_rp')))
        //     # rdp will be the instance associated to the class of fun
        //     self.reg('r_rdp', self.ctor_rdp_for(self.reg('r_rp'), fun))
        //     #...updating env if exist
        //     if self.reg('r_ep') != None:
        //         self.reg('r_ep')["r_rp"] = self.reg('r_rp')
        //         self.reg('r_ep')["r_rdp"] = self.reg('r_rdp')
        //     #...fun will be executed with this new r_rp, below
        // def evaluate(skip):
        //     if skip == True:
        //         self.state = 'running'
        //     self.evaluator = Eval([fun["compiled_function"]["body"]])
        //     self.evaluator.i = self
        //     try:
        //         ret,err = self.evaluator.apply("exec_fun")
        //         self.tear_fun()
        //     except ReturnException as e:
        //         self.tear_fun()
        //         ret = e.val
        //     except NonLocalReturnException as e:
        //         self.tear_fun()
        //         cp = self.reg('r_cp')
        //         if cp and not id_eq(cp['compiled_function'], e.top_level_cfun):
        //             raise
        //         else:
        //             raise ReturnException(e.val)
        //     except RewindException as e:
        //         if e.count > 1:
        //             e.count = e.count - 1
        //             self.tear_fun()
        //             raise e
        //         return self.run_fun(recv, drecv, fun, args, should_allocate)
        //     except MemetalkException:
        //         self.tear_fun()
        //         raise
        //     except Exception as e:
        //         self.tear_fun()
        //         raise
        //     if skip:
        //         self.state = 'paused'
        //     return ret
        // ret = evaluate(self.state == 'next')
        // return ret

}

// ------------------- VM -------------------

static int filepath_type(std::string filepath) {
  //-1: mmc
  // 1: mmi
  // 0: error
  if (filepath.size() <= 0) {
    return 0;
  }
  if (filepath[filepath.size()-1] == 'i') {
    return 1;
  }
  if (filepath[filepath.size()-1] == 'c') {
    return -1;
  }
  return 0;
}

static std::string read_file(char* filepath) {
  std::fstream file;
  file.open (filepath, std::fstream::out | std::fstream::binary);

  if (!file.is_open()) {
    bail("file not found");
  }

  file.seekg (0, std::ios::end);
  long size = file.tellg();

  debug() << "file size: "<< size << "\n";

  std::string contents;
  file >> contents;
  file.close();
  return contents;
}

VM::VM() {
}

int VM::start(char* filepath) {
  VMProcess* p = new VMProcess(this);
  _processes.push_back(p);

  switch(filepath_type(filepath)) {
    case -1:
      bail( ".mmc: not implemented");
      //return p->run_module((oop)read_file(filepath));
    case 1:
      return p->run_image(read_file(filepath));
  }
  return 0;
}

oop VM::heap_store(void* data) {
  _heap.push_back(data);
}
