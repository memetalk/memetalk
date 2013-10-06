#include <iostream>
#include <stdlib.h>
#include "vm.hpp"


int main(int argc, char** argv) {
  if (argc < 2) {
    std::cout << "Usage:\n";
    std::cout << "       sf-vm    <filename>.mmc\n";
    std::cout << "       sf-vm    <filename>.mmi\n";
    exit(0);
  }

  VM* vm = new VM;
  return vm->start(argv[1]);
}
