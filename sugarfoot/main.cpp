#include "vm.hpp"
#include <cstdlib>
#include <exception>
#include <iostream>

int main(int argc, char** argv) {
  char* online = getenv("ONLINE");
  char* repl = getenv("MM_REPL");
  try {
    VM* vm = new VM(argc, argv, !!online, "core.img");
    if (!!repl) {
      return vm->remote_repl();
    } else {
      return vm->start();
    }
  }  catch(const std::exception& e) {
    std::cerr << "Uncaugh C++ exception: " << e.what() << std::endl;
    return 1;
  }
}
