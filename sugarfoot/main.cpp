#include "vm.hpp"

int main(int argc, char** argv) {
  return VM(argc, argv, "core.img").start();
}
