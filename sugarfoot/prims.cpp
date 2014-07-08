#include "prims.hpp"
#include "vm.hpp"
#include <iostream>

void init_primitives(VM* vm) {
  vm->register_primitive("main", prim_main);
}


oop prim_main(Process*) {
  std::cout << "Hello world\n";
  return NULL;
}
