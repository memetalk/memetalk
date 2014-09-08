#include "vm.hpp"
#include <cstdlib>

int main(int argc, char** argv) {
  char* online = getenv("ONLINE");
  return (new VM(argc, argv, !!online, "core.img"))->start();
}
