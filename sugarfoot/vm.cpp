#include "vm.hpp"
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <string.h>
#include "core_loader.hpp"
#include "mmc_loader.hpp"
#include "report.hpp"

VM::VM(const char* core_img_filepath)
  : _core_image(new CoreImage(core_img_filepath)) {
}

int VM::start(char* filepath) {
  _core_image->load();

  MMCImage* mmc = new MMCImage(_core_image, filepath);
  mmc->load();

  return 0;
}

int main(int argc, char** argv) {
  if (argc != 2) {
    bail("usage: sf-vm <file.mmc>");
  }
  return VM("core.img").start(argv[1]);
}
