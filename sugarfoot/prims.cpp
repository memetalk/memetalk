#include "prims.hpp"
#include "vm.hpp"
#include "report.hpp"
#include "defs.hpp"
#include "process.hpp"
#include <assert.h>
#include "utils.hpp"
#include "mmobj.hpp"
#include "mmc_image.hpp"
#include "qt_prims.hpp"
#include <string>
#include <iostream>
#include <sstream>
#include <boost/filesystem.hpp>

namespace fs = ::boost::filesystem;

static int prim_io_print(Process* proc) {
  oop obj = *((oop*) proc->fp() - 1);

  debug() << "---- prim_print" << endl;
  int exc;
  oop res = proc->send_0(obj, proc->vm()->new_symbol("toString"), &exc);
  if (exc != 0) {
    proc->stack_push(res);
    return PRIM_RAISED;
  }
  std::cout << proc->mmobj()->mm_string_cstr(res) << endl;
  proc->stack_push(MM_NULL);
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

static int prim_string_equal(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  char* str_1 = proc->mmobj()->mm_string_cstr(self);
  char* str_2 = proc->mmobj()->mm_string_cstr(other);

  if (strcmp(str_1, str_2) == 0) {
    proc->stack_push(MM_TRUE);
  } else {
    proc->stack_push(MM_FALSE);
  }
  return 0;
}

static int prim_string_size(Process* proc) {
  oop self =  proc->dp();
  char* str_1 = proc->mmobj()->mm_string_cstr(self);
  int len = strlen(str_1);
  proc->stack_push(tag_small_int(len));
  return 0;
}

static int prim_string_count(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  char* str_1 = proc->mmobj()->mm_string_cstr(self);
  char* str_2 = proc->mmobj()->mm_string_cstr(other);
  int count = 0;
  char* pos = str_1;
  while ((pos = strstr(pos, str_2)) != NULL) {
    count++;
    pos++;
  }
  proc->stack_push(tag_small_int(count));
  return 0;
}

static int prim_string_rindex(Process* proc) {
  oop self =  proc->dp();
  oop arg = *((oop*) proc->fp() - 1);
  std::string str = proc->mmobj()->mm_string_cstr(self);
  std::string str_arg = proc->mmobj()->mm_string_cstr(arg);
  std::size_t pos = str.rfind(str_arg);
  if (pos == std::string::npos) {
    proc->stack_push(tag_small_int(-1));
  } else {
    proc->stack_push(tag_small_int(pos));
  }
  return 0;
}

static int prim_string_from(Process* proc) {
  oop self =  proc->dp();
  oop idx = *((oop*) proc->fp() - 1);
  std::string str = proc->mmobj()->mm_string_cstr(self);
  std::string sub = str.substr(untag_small_int(idx));
  proc->stack_push(proc->mmobj()->mm_string_new(sub.c_str()));
  return 0;
}

#include <boost/algorithm/string/replace.hpp>
static int prim_string_replace_all(Process* proc) {
  oop self =  proc->dp();
  oop val = *((oop*) proc->fp() - 1);
  oop what = *((oop*) proc->fp() - 2);

  std::string str = proc->mmobj()->mm_string_cstr(self);
  std::string _what = proc->mmobj()->mm_string_cstr(what);
  std::string _val = proc->mmobj()->mm_string_cstr(val);

  std::string output = boost::replace_all_copy(str, _what, _val);

  proc->stack_push(proc->mmobj()->mm_string_new(output.c_str()));
  return 0;
}


static int prim_number_sum(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  number res = untag_small_int(self) + untag_small_int(other);
  proc->stack_push((oop) tag_small_int(res)); //TODO: check for overflow
  return 0;
}

static int prim_number_sub(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  number res =  untag_small_int(self) - untag_small_int(other);
  debug() << " SUB " << untag_small_int(self) << " - " << untag_small_int(other) << " = " << res << endl;
  proc->stack_push((oop) tag_small_int(res));
  return 0;
}

static int prim_number_mul(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  number res =  untag_small_int(self) * untag_small_int(other);
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

static int prim_number_to_source(Process* proc) {
  return prim_number_to_string(proc);
}

static int prim_exception_throw(Process* proc) {
  oop self =  proc->rp();
  proc->stack_push(self);
  return PRIM_RAISED;
}

static int prim_list_new(Process* proc) {
  oop self = proc->mmobj()->mm_list_new();

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
    int exc;
    oop val = proc->do_call(fun, &exc);
    if (exc != 0) {
      debug() << "prim_list_each raised" << endl;
      proc->stack_push(val);
      return PRIM_RAISED;
    }
    debug() << "list each[" << i << "] fun returned " << val << endl;
  }
  proc->stack_push(self);
  return 0;
}

static int prim_list_has(Process* proc) {
  oop self =  proc->dp();
  oop value = *((oop*) proc->fp() - 1);
  if (proc->mmobj()->mm_list_index_of(self, value) == -1) {
    proc->stack_push(MM_FALSE);
  } else {
    proc->stack_push(MM_TRUE);
  }
  return 0;
}

static int prim_list_to_string(Process* proc) {
  oop self =  proc->dp();
  std::stringstream s;
  s << "[";
  std::string comma = "";
  for (int i = 0; i < proc->mmobj()->mm_list_size(self); i++) {
    int exc;
    oop res = proc->send_0(proc->mmobj()->mm_list_entry(self, i),
                                proc->vm()->new_symbol("toString"), &exc);
    if (exc != 0) {
      proc->stack_push(res);
      return PRIM_RAISED;
    }
    s << comma << proc->mmobj()->mm_string_cstr(res);
    comma = ", ";
  }
  s << "]";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_list_to_source(Process* proc) {
  oop self =  proc->dp();
  std::stringstream s;
  s << "[";
  std::string comma = "";
  for (int i = 0; i < proc->mmobj()->mm_list_size(self); i++) {
    int exc;
    oop res = proc->send_0(proc->mmobj()->mm_list_entry(self, i),
                                proc->vm()->new_symbol("toSource"), &exc);
    if (exc != 0) {
      proc->stack_push(res);
      return PRIM_RAISED;
    }
    s << comma << proc->mmobj()->mm_string_cstr(res);
    comma = ", ";
  }
  s << "]";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}


static int prim_dictionary_new(Process* proc) {
  oop self = proc->mmobj()->mm_dictionary_new();
  debug() << "Dictionary::new " << self << endl;
  proc->stack_push(self);
  return 0;
}

static int prim_dictionary_set(Process* proc) {
  oop self =  proc->dp();
  oop val = *((oop*) proc->fp() - 1);
  oop key = *((oop*) proc->fp() - 2);

  proc->mmobj()->mm_dictionary_set(self, key, val);
  proc->stack_push(self);
  return 0;
}

static int prim_dictionary_index(Process* proc) {
  oop self =  proc->dp();
  oop key = *((oop*) proc->fp() - 1);

  proc->stack_push(proc->mmobj()->mm_dictionary_get(self, key));
  return 0;
}

static int prim_dictionary_to_string(Process* proc) {
  oop self =  proc->dp();
  std::stringstream s;
  s << "{";
  std::map<oop, oop>::iterator it = proc->mmobj()->mm_dictionary_begin(self);
  std::map<oop, oop>::iterator end = proc->mmobj()->mm_dictionary_end(self);
  std::string comma = "";
  for ( ; it != end; it++) {
    int exc;
    oop res = proc->send_0(it->first,
                                proc->vm()->new_symbol("toString"), &exc);
    if (exc != 0) {
      proc->stack_push(res);
      return PRIM_RAISED;
    }
    s << comma << proc->mmobj()->mm_string_cstr(res) << ": ";

    res = proc->send_0(it->second,
                                proc->vm()->new_symbol("toString"), &exc);
    if (exc != 0) {
      proc->stack_push(res);
      return PRIM_RAISED;
    }
    s << proc->mmobj()->mm_string_cstr(res);
    comma = ", ";
  }
  s << "}";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_dictionary_to_source(Process* proc) {
  oop self =  proc->dp();
  std::stringstream s;
  s << "{";
  std::map<oop, oop>::iterator it = proc->mmobj()->mm_dictionary_begin(self);
  std::map<oop, oop>::iterator end = proc->mmobj()->mm_dictionary_end(self);
  std::string comma = "";
  for ( ; it != end; it++) {
    int exc;
    oop res = proc->send_0(it->first,
                                proc->vm()->new_symbol("toSource"), &exc);
    if (exc != 0) {
      proc->stack_push(res);
      return PRIM_RAISED;
    }
    s << comma << proc->mmobj()->mm_string_cstr(res) << ": ";

    res = proc->send_0(it->second,
                                proc->vm()->new_symbol("toSource"), &exc);
    if (exc != 0) {
      proc->stack_push(res);
      return PRIM_RAISED;
    }
    s << proc->mmobj()->mm_string_cstr(res);
    comma = ", ";
  }
  s << "}";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_dictionary_plus(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  oop d = proc->mmobj()->mm_dictionary_new();

  std::map<oop, oop>::iterator it = proc->mmobj()->mm_dictionary_begin(self);
  std::map<oop, oop>::iterator end = proc->mmobj()->mm_dictionary_end(self);
  for ( ; it != end; it++) {
    proc->mmobj()->mm_dictionary_set(d, it->first, it->second);
  }

  it = proc->mmobj()->mm_dictionary_begin(other);
  end = proc->mmobj()->mm_dictionary_end(other);
  for ( ; it != end; it++) {
    proc->mmobj()->mm_dictionary_set(d, it->first, it->second);
  }
  proc->stack_push(d);
  return 0;
}

static int prim_dictionary_has(Process* proc) {
  oop self =  proc->dp();
  oop key = *((oop*) proc->fp() - 1);
  if (proc->mmobj()->mm_dictionary_has_key(self, key)) {
    proc->stack_push(MM_TRUE);
  } else {
    proc->stack_push(MM_FALSE);
  }
  return 0;
}

static int prim_mirror_entries(Process* proc) {
  oop self =  proc->dp();
  oop mirrored = ((oop*)self)[2];


  if (is_small_int(mirrored)) {
    debug() << "prim_mirror_entries: mirrored is number" << endl;
    oop lst = proc->mmobj()->mm_list_new();
    proc->stack_push(lst);
    return 0;
  } else if (proc->mmobj()->mm_is_list(mirrored)) {
      oop lst = proc->mmobj()->mm_list_new();
      for (number i = 0; i < proc->mmobj()->mm_list_size(mirrored); i++) {
        proc->mmobj()->mm_list_append(lst, tag_small_int(i));
      }
      proc->stack_push(lst);
      return 0;
  } else if (proc->mmobj()->mm_is_dictionary(mirrored)) {
      oop lst = proc->mmobj()->mm_list_new();

      std::map<oop, oop>::iterator it = proc->mmobj()->mm_dictionary_begin(mirrored);
      std::map<oop, oop>::iterator end = proc->mmobj()->mm_dictionary_end(mirrored);
      for ( ; it != end; it++) {
        proc->mmobj()->mm_list_append(lst, it->first);
      }
      proc->stack_push(lst);
      return 0;
  } else {
    debug() << "prim_mirror_entries: mirrored is not [number|list|dict]" << endl;

    oop mirrored_class = proc->mmobj()->mm_object_vt(mirrored);

    if (proc->mmobj()->delegates_to(mirrored_class, proc->vm()->get_prime("Object"))) {
      // mirrored is a class instance
      debug() << "prim_mirror_entries: mirrored is class instance" << endl;
      oop cclass = proc->mmobj()->mm_class_get_compiled_class(mirrored_class);
      proc->stack_push(proc->mmobj()->mm_compiled_class_fields(cclass));
      return 0;
    } else { //unknown structure
      debug() << "prim_mirror_entries: mirrored has unknown structure" << endl;
      oop lst = proc->mmobj()->mm_list_new();
      proc->stack_push(lst);
      return 0;
    }
  }
}

static int prim_mirror_value_for(Process* proc) {
  oop self =  proc->dp();
  oop entry = *((oop*) proc->fp() - 1);

  oop mirrored = ((oop*)self)[2];

  if (proc->mmobj()->mm_is_list(mirrored)) {
    proc->stack_push(proc->mmobj()->mm_list_entry(mirrored, untag_small_int(entry)));
    return 0;
  } else if (proc->mmobj()->mm_is_dictionary(mirrored)) {
    proc->stack_push(proc->mmobj()->mm_dictionary_get(mirrored, entry));
    return 0;
  } else { //generic instance ?
    //assume mirrored is a class instance
    oop mirrored_class = proc->mmobj()->mm_object_vt(mirrored);
    assert(proc->mmobj()->delegates_to(mirrored_class, proc->vm()->get_prime("Object")));

    oop cclass = proc->mmobj()->mm_class_get_compiled_class(mirrored_class);
    oop fields = proc->mmobj()->mm_compiled_class_fields(cclass);

    number idx = proc->mmobj()->mm_list_index_of(fields, entry);
    assert(idx >= 0);
    debug() << "prim_mirror_value_for index of " << idx << endl;
    proc->stack_push(((oop*)mirrored)[2+idx]);
    return 0;
  }
}

static int prim_mirror_set_value_for(Process* proc) {
  oop self =  proc->dp();
  oop value = *((oop*) proc->fp() - 1);
  oop entry = *((oop*) proc->fp() - 2);

  oop mirrored = ((oop*)self)[2];

  if (proc->mmobj()->mm_is_list(mirrored)) {
    proc->mmobj()->mm_list_set(mirrored, untag_small_int(entry), value);
    proc->stack_push(proc->rp());
    return 0;
  } else if (proc->mmobj()->mm_is_dictionary(mirrored)) {
    proc->mmobj()->mm_dictionary_set(mirrored, entry, value);
    return 0;
  } else { //generic instance ?
    //assume mirrored is a class instance
    oop mirrored_class = proc->mmobj()->mm_object_vt(mirrored);
    assert(proc->mmobj()->delegates_to(mirrored_class, proc->vm()->get_prime("Object")));

    oop cclass = proc->mmobj()->mm_class_get_compiled_class(mirrored_class);
    oop fields = proc->mmobj()->mm_compiled_class_fields(cclass);

    number idx = proc->mmobj()->mm_list_index_of(fields, entry);
    assert(idx >= 0);
    debug() << "prim_mirror_set_value_for index of " << idx << endl;
    ((oop*)mirrored)[2+idx] = value;
    proc->stack_push(proc->rp());
    return 0;
  }
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

static int prim_behavior_to_source(Process* proc) {
  return prim_behavior_to_string(proc);
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

static int prim_object_to_source(Process* proc) {
  return prim_object_to_string(proc);
}

static int prim_symbol_to_string(Process* proc) {
  oop self =  proc->dp();
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
  s << "#<" << str_mod_name << " module instance: " << self << ">";
  oop oop_str = proc->mmobj()->mm_string_new(s.str().c_str());
  proc->stack_push(oop_str);
  return 0;
}

static int prim_compiled_function_with_env(Process* proc) {
  // oop self =  proc->dp();
  oop cmod = *((oop*) proc->fp() - 1);
  oop vars = *((oop*) proc->fp() - 2);
  oop text = *((oop*) proc->fp() - 3);

  debug() << "prim_compiled_function_with_env " << proc->mmobj()->mm_string_cstr(text) << " -- " << cmod << " " << vars << endl;
  int exc;
  oop cfun = proc->vm()->compile_fun(proc->mmobj()->mm_string_cstr(text), vars, cmod, &exc);
  if (exc != 0) {
    proc->stack_push(cfun);
    return PRIM_RAISED;
  }

  debug() << "prim_compiled_function_with_env: GOT cfun: " << cfun << " " << *(oop*) cfun << endl;
  proc->stack_push(cfun);
  return 0;
}

static int prim_compiled_function_as_context_with_vars(Process* proc) {
  /* (1) allocate and assemble the ep/env from the vars with its values;
     (2) instantiate a Context with (self, env, imod)
  */

  oop self =  proc->dp();
  oop vars = *((oop*) proc->fp() - 1);
  oop imod = *((oop*) proc->fp() - 2);

  number env_size = proc->mmobj()->mm_compiled_function_get_num_locals_or_env(self);
  oop env = (oop) calloc(sizeof(oop), env_size + 2); //+2: rp, dp

  if (vars != MM_NULL) {
    oop env_table = proc->mmobj()->mm_compiled_function_env_table(self);
    std::map<oop,oop>::iterator it = proc->mmobj()->mm_dictionary_begin(vars);
    std::map<oop,oop>::iterator end = proc->mmobj()->mm_dictionary_end(vars);
    for ( ; it != end; it++) {
      std::string name = proc->mmobj()->mm_symbol_cstr(it->first);
      if (name == "this") {
        ((oop*)env)[0] = it->second; //rp
        ((oop*)env)[1] = it->second; //ep
      } else {
        number idx = untag_small_int(proc->mmobj()->mm_dictionary_get(env_table, it->first));
        ((oop*)env)[idx+2] = it->second;
      }
    }
  }

  oop args = proc->mmobj()->mm_list_new();
  proc->mmobj()->mm_list_append(args, self);
  proc->mmobj()->mm_list_append(args, env);
  proc->mmobj()->mm_list_append(args, imod);

  debug() << "compiled_function_as_context_with_vars: Creating context..." << self << " " << vars << " " << imod << endl;

  int exc;
  oop ctx = proc->send(proc->vm()->get_prime("Context"), proc->vm()->new_symbol("new"), args, &exc);
  if (exc != 0) {
    proc->stack_push(ctx);
    return PRIM_RAISED;
  }

  // debug() << "compiled_function_as_context_with_vars: " << ctx << endl;
  // debug() << "ctx[cfun] " << proc->mmobj()->mm_function_get_cfun(ctx) << endl;
  // debug() << "ctx[env] " << proc->mmobj()->mm_context_get_env(ctx) << endl;
  // debug() << "ctx[imod] " << proc->mmobj()->mm_function_get_module(ctx) << endl;

  proc->stack_push(ctx);
  return 0;
}

static int prim_context_get_env(Process* proc) {
  oop self =  proc->dp();

  oop ctx_env = proc->mmobj()->mm_context_get_env(self);

  oop env_table = proc->mmobj()->mm_function_env_table(self);

  oop env_dict = proc->mmobj()->mm_dictionary_new();


  std::map<oop, oop>::iterator it = proc->mmobj()->mm_dictionary_begin(env_table);
  std::map<oop, oop>::iterator end = proc->mmobj()->mm_dictionary_end(env_table);
  for ( ; it != end; it++) {
    number idx = untag_small_int(it->second);
    debug() << "env idx " << idx << endl;
    proc->mmobj()->mm_dictionary_set(env_dict, it->first, ((oop*)ctx_env)[2+idx]); //2: rp, dp
  }

  proc->stack_push(env_dict);
  return 0;
}

static int prim_function_get_env(Process* proc) {
  return prim_context_get_env(proc);
}

static int prim_get_compiled_module(Process* proc) {
  oop imod = *((oop*) proc->fp() - 1);
  proc->stack_push(proc->mmobj()->mm_module_get_cmod(imod));
  return 0;
}

static int prim_test_import(Process* proc) {
  oop args = *((oop*) proc->fp() - 1);
  oop filepath = *((oop*) proc->fp() - 2);

  char* str_filepath = proc->mmobj()->mm_string_cstr(filepath);
  debug () << str_filepath << endl;
  MMCImage* mmc = new MMCImage(proc, proc->vm()->core(), str_filepath);
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

  oop list = proc->mmobj()-> mm_list_new();

  for(std::vector<std::string>::iterator it = ret.begin(); it != ret.end(); it++) {
    oop str = proc->mmobj()->mm_string_new((*it).c_str());
    proc->mmobj()->mm_list_append(list, str);
  }

  proc->stack_push(list);
  return 0;
}

static int prim_test_catch_exception(Process* proc) {
  int exc;
  oop ret = proc->send_0(proc->mp(), proc->vm()->new_symbol("bar"), &exc);
  proc->stack_push(ret);
  return 0;
}

static int prim_modules_path(Process* proc) {
  const char* mmpath = getenv("MEME_PATH");
  if (mmpath) {
    proc->stack_push(proc->mmobj()->mm_string_new(mmpath));
  } else {
    proc->stack_push(proc->mmobj()->mm_string_new("./mm"));
  }
  return 0;
}

void init_primitives(VM* vm) {
  vm->register_primitive("io_print", prim_io_print);

  vm->register_primitive("behavior_to_string", prim_behavior_to_string);
  vm->register_primitive("behavior_to_source", prim_behavior_to_source);

  vm->register_primitive("number_sum", prim_number_sum);
  vm->register_primitive("number_sub", prim_number_sub);
  vm->register_primitive("number_mul", prim_number_mul);
  vm->register_primitive("number_lt", prim_number_lt);
  vm->register_primitive("number_to_string", prim_number_to_string);
  vm->register_primitive("number_to_source", prim_number_to_source);

  vm->register_primitive("exception_throw", prim_exception_throw);

  vm->register_primitive("equal", prim_equal);
  // vm->register_primitive("id", prim_id);

  vm->register_primitive("object_not", prim_object_not);
  vm->register_primitive("object_to_string", prim_object_to_string);
  vm->register_primitive("object_to_source", prim_object_to_source);

  vm->register_primitive("symbol_to_string", prim_symbol_to_string);

  vm->register_primitive("module_to_string", prim_module_to_string);

  vm->register_primitive("list_new", prim_list_new);
  vm->register_primitive("list_append", prim_list_append);
  vm->register_primitive("list_prepend", prim_list_prepend);
  vm->register_primitive("list_index", prim_list_index);
  vm->register_primitive("list_each", prim_list_each);
  vm->register_primitive("list_has", prim_list_has);
  vm->register_primitive("list_to_string", prim_list_to_string);
  vm->register_primitive("list_to_source", prim_list_to_source);

  vm->register_primitive("dictionary_new", prim_dictionary_new);
  vm->register_primitive("dictionary_set", prim_dictionary_set);
  vm->register_primitive("dictionary_index", prim_dictionary_index);
  vm->register_primitive("dictionary_plus", prim_dictionary_plus);
  vm->register_primitive("dictionary_has", prim_dictionary_has);
  vm->register_primitive("dictionary_to_string", prim_dictionary_to_string);
  vm->register_primitive("dictionary_to_source", prim_dictionary_to_source);

  vm->register_primitive("string_append", prim_string_append);
  vm->register_primitive("string_equal", prim_string_equal);
  vm->register_primitive("string_count", prim_string_count);
  vm->register_primitive("string_size", prim_string_size);
  vm->register_primitive("string_rindex", prim_string_rindex);
  vm->register_primitive("string_from", prim_string_from);
  vm->register_primitive("string_replace_all", prim_string_replace_all);


  vm->register_primitive("mirror_entries", prim_mirror_entries);
  vm->register_primitive("mirror_value_for", prim_mirror_value_for);
  vm->register_primitive("mirror_set_value_for", prim_mirror_set_value_for);


  vm->register_primitive("compiled_function_with_env", prim_compiled_function_with_env);
  vm->register_primitive("compiled_function_as_context_with_vars", prim_compiled_function_as_context_with_vars);
  vm->register_primitive("context_get_env", prim_context_get_env);
  vm->register_primitive("function_get_env", prim_function_get_env);


  vm->register_primitive("test_import", prim_test_import);
  vm->register_primitive("test_get_module_function", prim_test_get_module_function);
  vm->register_primitive("test_files", prim_test_files);

  vm->register_primitive("test_catch_exception", prim_test_catch_exception);

  vm->register_primitive("get_compiled_module", prim_get_compiled_module);

  vm->register_primitive("modules_path", prim_modules_path);

  qt_init_primitives(vm);
}
