#include "prims.hpp"
#include "vm.hpp"
#include "report.hpp"
#include "defs.hpp"
#include "process.hpp"
#include <assert.h>
#include "utils.hpp"

static oop prim_main(Process*) {
  debug() << "Hello world\n";
  return NULL;
}

static oop prim_number_sum(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  oop res =  (oop) (untag_small_int(self) + untag_small_int(other));
  return (oop) tag_small_int(res); //TODO: check for overflow
}

static oop prim_number_mul(Process* proc) {
  oop self =  proc->dp();
  oop other = *((oop*) proc->fp() - 1);

  assert(is_small_int(self));
  assert(is_small_int(other));

  oop res =  (oop) (untag_small_int(self) * untag_small_int(other));
  return (oop) tag_small_int(res); //TODO: check for overflow
}

void init_primitives(VM* vm) {
  vm->register_primitive("main", prim_main);
  vm->register_primitive("number_sum", prim_number_sum);
  vm->register_primitive("number_mul", prim_number_mul);
}
