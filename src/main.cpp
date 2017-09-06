#include "vm.hpp"
#include <cstdlib>
#include <exception>
#include <iostream>

#include <gc_cpp.h>
#include "gc/gc_allocator.h"
#include <boost/filesystem.hpp>


std::string core_path(char** argv) {
  char buf[PATH_MAX];
  char *res = realpath(argv[0], buf);
  if (!res) {
    std::cerr << "could not get current directory\n";
    exit(EXIT_FAILURE);
  }
  boost::filesystem::path p1(res);
  p1.remove_filename();
  return p1.string() + "/core.img";
}

int main(int argc, char** argv) {
  GC_INIT();
  char* online = getenv("ONLINE");
  char* profile = getenv("PROFILE");
  try {
    VM* vm = new (GC) VM(argc, argv, !!online, !!profile, core_path(argv));
    return vm->start();
  } catch(const std::exception& e) {
    std::cerr << "Uncaugh C++ exception: " << e.what() << std::endl;
    return 1;
  }
}
