#include "prims.hpp"
#include "vm.hpp"
#include "report.hpp"
#include "defs.hpp"
#include "process.hpp"
#include <assert.h>
#include "utils.hpp"
#include "mmobj.hpp"
#include "mmc_image.hpp"
#include <string>
#include <iostream>
#include <sstream>
#include <boost/filesystem.hpp>

namespace fs = ::boost::filesystem;

static int prim_io_print(Process* proc) {
  oop obj = *((oop*) proc->fp() - 1);

  debug() << "---- prim_print" << endl;
  oop oop_str = proc->do_send_0(obj, proc->vm()->new_symbol("toString"));
  debug() << "---- prim_print do_send_0 done " << oop_str << endl;
  std::cout << proc->mmobj()->mm_string_cstr(oop_str) << endl;
  proc->stack_push((oop)0);
  return 0;
}

static int prim_string_append(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  char* str_1 = proc->mmobj()->mm_string_cstr(self);
  char* str_2 = proc->mmobj()->mm_string_cstr(other);

  std::stringstream s;
  s << str_1 << str_2;
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_number_sum(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  oop res =  (oop) (untag_small_int(self) + untag_small_int(other));
  proc->stack_push((oop) tag_small_int(res)); //TODO: check for overflow
  return 0;
}

static int prim_number_sub(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  oop res =  (oop) (untag_small_int(self) - untag_small_int(other));
  debug() << " SUB " << untag_small_int(self) << " - " << untag_small_int(other) << " = " << res << endl;
  proc->stack_push((oop) tag_small_int(res));
  return 0;
}

static int prim_number_mul(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  oop res =  (oop) (untag_small_int(self) * untag_small_int(other));
  proc->stack_push((oop) tag_small_int(res));  //TODO: check for overflow
  return 0;
}

static int prim_number_lt(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  oop res =  (oop) (untag_small_int(self) < untag_small_int(other));
  debug() << " PRIM< " << untag_small_int(self) << " < " << untag_small_int(other) << " = " << (untag_small_int(self) < untag_small_int(other)) << endl;
  proc->stack_push((oop)res);
  return 0;
}

static int prim_number_to_string(Process* proc) {
  oop self =  proc->dp();
  std::stringstream s;
  s << untag_small_int(self);
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_exception_throw(Process* proc) {
  oop self =  proc->rp();
  proc->stack_push(self);
  return PRIM_RAISED;
}

static int prim_list_new(Process* proc) {
  oop self = proc->mmobj()->mm_list_new_empty();

  debug() << "List::new " << self << endl;
  proc->stack_push(self);
  return 0;
}

static int prim_list_append(Process* proc) {
  oop self =  proc->dp();
  oop element = *((oop*) proc->fp() - 1);

  proc->mmobj()->mm_list_append(self, element);

  proc->stack_push(self);
  return 0;
}

static int prim_list_prepend(Process* proc) {
  oop self =  proc->dp();
  oop element = *((oop*) proc->fp() - 1);

  proc->mmobj()->mm_list_prepend(self, element);
  proc->stack_push(self);
  return 0;
}

static int prim_list_index(Process* proc) {
  oop self =  proc->dp();
  oop index_oop = *((oop*) proc->fp() - 1);
  number index = untag_small_int(index_oop);

  oop val = proc->mmobj()->mm_list_entry(self, index);
  debug() << "list " << self << "[" << index << "] = " << val << endl;
  proc->stack_push(val);
  return 0;
}

static int prim_list_each(Process* proc) {
  oop self =  proc->dp();
  oop fun = *((oop*) proc->fp() - 1);

  number size = proc->mmobj()->mm_list_size(self);

  for (int i = 0; i < size; i++) {
    oop next = proc->mmobj()->mm_list_entry(self, i);
    debug() << "list each[" << i << "] = " << next << endl;
    proc->stack_push(next);
    oop val = proc->do_call(fun);
    debug() << "list each[" << i << "] fun returned " << val << endl;
  }
  proc->stack_push(self);
  return 0;
}

static int prim_list_to_string(Process* proc) {
  oop self =  proc->rp();
  std::stringstream s;
  s << "[";
  for (int i = 0; i < proc->mmobj()->mm_list_size(self); i++) {
    oop tostr = proc->do_send_0(proc->mmobj()->mm_list_entry(self, i),
                              proc->vm()->new_symbol("toString"));
    s << proc->mmobj()->mm_string_cstr(tostr) << ",";
  }
  s << "]";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_equal(Process* proc) {
  oop self =  proc->rp();
  oop other = *((oop*) proc->fp() - 1);
  debug() << self << " == " << other << "?" << (self == other ? MM_TRUE : MM_FALSE) << endl;
  proc->stack_push(self == other ? MM_TRUE : MM_FALSE);
  return 0;
}

// static int prim_id(Process* proc) {
//   oop self =  proc->rp();
//   std::stringstream s;
//   s << self;
//   proc->stack_push(proc->mmobj()->mm_string_new(s.str().c_str()));
//   return 0;
// }

static int prim_object_not(Process* proc) {
  oop self =  proc->dp();
  if ((self == MM_FALSE) || (self == MM_NULL)) {
    proc->stack_push(MM_TRUE);
  } else {
    proc->stack_push(MM_FALSE);
  }
  return 0;
}

static int prim_behavior_to_string(Process* proc) {
  oop klass =  proc->rp();

  oop cclass = proc->mmobj()->mm_class_get_compiled_class(klass);
  oop class_name = proc->mmobj()->mm_compiled_class_name(cclass);
  char* str_class_name = proc->mmobj()->mm_string_cstr(class_name);
  std::stringstream s;
  s << "#<" << str_class_name << " class>";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_object_to_string(Process* proc) {
  oop self =  proc->rp();

  oop klass = proc->mmobj()->mm_object_vt(self);
  oop cclass = proc->mmobj()->mm_class_get_compiled_class(klass);
  oop class_name = proc->mmobj()->mm_compiled_class_name(cclass);
  char* str_class_name = proc->mmobj()->mm_string_cstr(class_name);
  std::stringstream s;
  s << "#<" << str_class_name << " instance: " << self << ">";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_symbol_to_string(Process* proc) {
  oop self =  proc->rp();
  char* str = proc->mmobj()->mm_symbol_cstr(self);
  debug() << "prim_symbol_to_string: " << self << " str: " << str << endl;
  oop oop_str = proc->mmobj()->mm_string_new(str);
  proc->stack_push(oop_str);
  return 0;
}

static int prim_module_to_string(Process* proc) {
  oop self =  proc->rp();

  oop cmod = proc->mmobj()->mm_module_get_cmod(self);
  // debug() << "prim_module_to_string imod: " << self << " cmod: " << cmod << endl;
  oop mod_name = proc->mmobj()->mm_compiled_module_name(cmod);
  char* str_mod_name = proc->mmobj()->mm_string_cstr(mod_name);
  std::stringstream s;
  s << "#<" << str_mod_name << " modile instance: " << self << ">";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_test_import(Process* proc) {
  oop args = *((oop*) proc->fp() - 1);
  oop filepath = *((oop*) proc->fp() - 2);

  char* str_filepath = proc->mmobj()->mm_string_cstr(filepath);
  debug () << str_filepath << endl;
  MMCImage* mmc = new MMCImage(proc->vm(), proc->vm()->core(), str_filepath);
  mmc->load();
  proc->stack_push(mmc->instantiate_module(args));
  return 0;
}

static int prim_test_get_module_function(Process* proc) {
  oop name = *((oop*) proc->fp() - 1);
  oop imod = *((oop*) proc->fp() - 2);

  // char* str = proc->mmobj()->mm_symbol_cstr(name);
  // debug() << "XX: " << name << " str: " << str << endl;

  oop dict = proc->mmobj()->mm_module_dictionary(imod);
  oop fun = proc->mmobj()->mm_dictionary_get(dict, name);
  debug() << "prim_test_get_module_function: " << imod << " "
          << name << " " << dict << " " << fun << endl;
  proc->stack_push(fun);
  return 0;
}

static
void get_mm_files(const fs::path& root, std::vector<std::string>& ret) {
  if (!fs::exists(root)) return;

  if (fs::is_directory(root)) {
    fs::directory_iterator it(root);
    fs::directory_iterator endit;
    while(it != endit) {
      if (fs::is_regular_file(*it) and it->path().extension() == ".mmc")
      {
        ret.push_back(it->path().string());
      }
      ++it;
    }
  }
}

static int prim_test_files(Process* proc) {
  std::vector<std::string> ret;
  get_mm_files("./tests", ret);

  oop list = proc->mmobj()-> mm_list_new_empty();

  for(std::vector<std::string>::iterator it = ret.begin(); it != ret.end(); it++) {
    oop str = proc->mmobj()->mm_string_new((*it).c_str());
    proc->mmobj()->mm_list_append(list, str);
  }

  proc->stack_push(list);
  return 0;
}

void init_primitives(VM* vm) {
  vm->register_primitive("io_print", prim_io_print);

  vm->register_primitive("behavior_to_string", prim_behavior_to_string);

  vm->register_primitive("number_sum", prim_number_sum);
  vm->register_primitive("number_sub", prim_number_sub);
  vm->register_primitive("number_mul", prim_number_mul);
  vm->register_primitive("number_lt", prim_number_lt);
  vm->register_primitive("number_to_string", prim_number_to_string);

  vm->register_primitive("exception_throw", prim_exception_throw);

  vm->register_primitive("equal", prim_equal);
  // vm->register_primitive("id", prim_id);

  vm->register_primitive("object_not", prim_object_not);
  vm->register_primitive("object_to_string", prim_object_to_string);
  vm->register_primitive("list_to_string", prim_list_to_string);

  vm->register_primitive("symbol_to_string", prim_symbol_to_string);

  vm->register_primitive("module_to_string", prim_module_to_string);

  vm->register_primitive("list_new", prim_list_new);
  vm->register_primitive("list_append", prim_list_append);
  vm->register_primitive("list_prepend", prim_list_prepend);
  vm->register_primitive("list_index", prim_list_index);
  vm->register_primitive("list_each", prim_list_each);

  vm->register_primitive("string_append", prim_string_append);

  vm->register_primitive("test_import", prim_test_import);
  vm->register_primitive("test_get_module_function", prim_test_get_module_function);
  vm->register_primitive("test_files", prim_test_files);
}
