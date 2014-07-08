#include "vm.hpp"
#include "report.hpp"

int main(int argc, char** argv) {
  if (argc != 2) {
    bail("usage: sf-vm <file.mmc>");
  }
  return VM("core.img").start(argv[1]);
}
