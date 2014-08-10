#include "vm.hpp"

int main(int argc, char** argv) {
  return (new VM(argc, argv, "core.img"))->start();
}
