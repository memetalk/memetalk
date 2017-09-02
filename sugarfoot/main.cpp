#include "vm.hpp"
#include <cstdlib>
#include <exception>
#include <iostream>

#include <gc_cpp.h>
#include "gc/gc_allocator.h"


int main(int argc, char** argv) {
  GC_INIT();
  char* online = getenv("ONLINE");
  char* profile = getenv("PROFILE");
  try {
    VM* vm = new (GC) VM(argc, argv, !!online, !!profile, "core.img");
    return vm->start();
  } catch(const std::exception& e) {
    std::cerr << "Uncaugh C++ exception: " << e.what() << std::endl;
    return 1;
  }
}
