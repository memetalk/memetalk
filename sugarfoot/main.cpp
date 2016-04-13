#include "vm.hpp"
#include <cstdlib>
#include <exception>
#include <iostream>

int main(int argc, char** argv) {
  char* online = getenv("ONLINE");
  try {
    return (new VM(argc, argv, !!online, "core.img"))->start();
  }  catch(const std::exception& e) {
    std::cerr << "Uncaugh C++ exception: " << e.what() << std::endl;
    return 1;
  }
}
