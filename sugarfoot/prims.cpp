#include "prims.hpp"
#include "vm.hpp"
#include "report.hpp"
#include "defs.hpp"
#include "process.hpp"
#include <assert.h>
#include "utils.hpp"
#include "mmobj.hpp"
#include <string.h>

static int prim_print(Process* proc) {
  oop obj = *((oop*) proc->fp() - 1);
  if (obj == NULL) {
    debug() << "* PRINT: NULL" << endl;
  } else if (proc->mmobj()->mm_is_string(obj)) {
    debug() << "* PRINT: " << proc->mmobj()->mm_string_cstr(obj) << endl;
  } else if (is_small_int(obj)) {
    debug() << "* PRINT: " << untag_small_int(obj) << endl;
  } else {
    debug() << "* PRINT: <#" << obj << ">" << endl;
  }
  proc->stack_push((oop)0);
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

static int prim_exception_throw(Process* proc) {
  oop self =  proc->dp();
  // oop ex = proc->mmobj()->mm_new(proc->rp(), proc->mmobj()->mm_object_new(), 1); // Exception < Object
  // ((oop*)ex)[2] = arg;
  proc->stack_push(self);
  return PRIM_RAISED;
}

static int prim_list_new(Process* proc) {
  oop self = proc->mmobj()->mm_new(proc->vm()->get_prime("List"),
                                   proc->mmobj()->mm_object_new(),
                                   2); //1=len, 2=firs element

  ((word*)self)[2] = 0;
  debug() << "List::new " << self << endl;
  proc->stack_push(self);
  return 0;
}

static int prim_list_append(Process* proc) {
  oop self =  proc->dp();
  oop element = *((oop*) proc->fp() - 1);

  number size = proc->mmobj()->mm_list_size(self);

  debug() << "List::append " << size << endl;

  oop begin;
  if (size == 0) {
    begin = (oop) calloc(sizeof(oop), size + 1);
    debug() << "alloc " << begin << " " << size + 1 << endl;
  } else {
    begin = proc->mmobj()->mm_list_frame(self);
    debug() << "reallocng " << begin << " " << size + 1 << endl;
    begin = (oop) realloc(begin, sizeof(oop) * (size + 1));
    debug() << "realloced " << begin << endl;
  }

  ((oop*)begin)[size] = element;

  proc->mmobj()->mm_list_set_size(self, size + 1);
  proc->mmobj()->mm_list_set_frame(self, begin);
  proc->stack_push(self);
  return 0;
}

static int prim_list_prepend(Process* proc) {
  oop self =  proc->dp();
  oop element = *((oop*) proc->fp() - 1);

  number size = proc->mmobj()->mm_list_size(self);

  debug() << "List::prepend " << size << endl;

  oop begin = (oop) malloc(sizeof(oop) * (size + 1));
  if (size > 0) {
    oop old_frame = proc->mmobj()->mm_list_frame(self);
    debug() << "moving mem from " << old_frame << " to " << begin << endl;
    for (int i = 0; i < size; i++) {
      begin[i+1] = old_frame[i];
    }
    free(old_frame);
    debug() << "moving mem got " << begin << endl;
  }

  ((oop*)begin)[0] = element;

  proc->mmobj()->mm_list_set_size(self, size + 1);
  proc->mmobj()->mm_list_set_frame(self, begin);
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

static int prim_object_not(Process* proc) {
  oop self =  proc->dp();
  if ((self == MM_FALSE) || (self == MM_NULL)) {
    proc->stack_push(MM_TRUE);
  } else {
    proc->stack_push(MM_FALSE);
  }
  return 0;
}

void init_primitives(VM* vm) {
  vm->register_primitive("print", prim_print);
  vm->register_primitive("number_sum", prim_number_sum);
  vm->register_primitive("number_sub", prim_number_sub);
  vm->register_primitive("number_mul", prim_number_mul);
  vm->register_primitive("number_lt", prim_number_lt);

  vm->register_primitive("exception_throw", prim_exception_throw);

  vm->register_primitive("object_not", prim_object_not);

  vm->register_primitive("list_new", prim_list_new);
  vm->register_primitive("list_append", prim_list_append);
  vm->register_primitive("list_prepend", prim_list_prepend);
  vm->register_primitive("list_index", prim_list_index);
  vm->register_primitive("list_each", prim_list_each);
}
