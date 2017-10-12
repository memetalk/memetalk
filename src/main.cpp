#include "vm.hpp"
#include <cstdlib>
#include <exception>
#include <iostream>

#include <gc_cpp.h>
#include "gc/gc_allocator.h"
#include <boost/filesystem.hpp>


int main(int argc, char** argv) {
  GC_INIT();
  char* online = getenv("ONLINE");
  char* profile = getenv("PROFILE");
  try {
    VM* vm = new (GC) VM(argc, argv, !!online, !!profile);
    return vm->start();
  } catch(const std::exception& e) {
    std::cerr << "Uncaugh C++ exception: " << e.what() << std::endl;
    return 1;
  }
}
