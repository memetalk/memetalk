#include "vm.hpp"
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <string.h>
#include "core_loader.hpp"
#include "mmc_loader.hpp"
#include "report.hpp"
#include "defs.hpp"
#include "mmobj.hpp"

VM::VM(const char* core_img_filepath)
  : _core_image(new CoreImage(core_img_filepath)) {
}

int VM::start(char* filepath) {
  _core_image->load();

  MMCImage* mmc = new MMCImage(this, _core_image, filepath);
  oop imod = mmc->load();
  return 0;
}

oop VM::new_symbol(const char* cstr) {
  std::string s = cstr;
  if (_symbols.find(s) == _symbols.end()) {
    _symbols[s] = mm_symbol_new(cstr, _core_image);
  }
  return _symbols[s];
}


//

int main(int argc, char** argv) {
  if (argc != 2) {
    bail("usage: sf-vm <file.mmc>");
  }
  return VM("core.img").start(argv[1]);
}
