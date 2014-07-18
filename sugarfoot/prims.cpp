#include "prims.hpp"
#include "vm.hpp"
#include "report.hpp"
#include "defs.hpp"
#include "process.hpp"
#include <assert.h>
#include "utils.hpp"
#include "mmobj.hpp"

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

void init_primitives(VM* vm) {
  vm->register_primitive("print", prim_print);
  vm->register_primitive("number_sum", prim_number_sum);
  vm->register_primitive("number_sub", prim_number_sub);
  vm->register_primitive("number_mul", prim_number_mul);
  vm->register_primitive("number_lt", prim_number_lt);

  vm->register_primitive("exception_throw", prim_exception_throw);
}
