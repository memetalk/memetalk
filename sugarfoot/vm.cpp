#include "vm.hpp"
#include "process.hpp"
#include "core_image.hpp"
#include "mmc_image.hpp"
#include "defs.hpp"
#include "mmobj.hpp"
#include "prims.hpp"
#include "report.hpp"
#include <assert.h>

VM::VM(const char* core_img_filepath)
  : _core_image(new CoreImage(core_img_filepath)), _mmobj(new MMObj(this, _core_image)) {
}

MMObj* VM::mmobj() {
  return _mmobj;
}

int VM::start(char* filepath) {
  _core_image->load();

  MMCImage* mmc = new MMCImage(this, _core_image, filepath);
  oop imod = mmc->load();

  init_primitives(this);

  _process = new Process(this);
  oop retval = _process->run(imod, imod, new_symbol("main"));
  if (_mmobj->mm_is_small_int(retval)) {
    debug() << "RETVAL " << _mmobj->mm_untag_small_int(retval) << endl;
  } else {
    debug() << "RETVAL " << retval << endl;
  }
  return 0;
}

oop VM::new_symbol(const char* cstr) {
  std::string s = cstr;
  if (_symbols.find(s) == _symbols.end()) {
    _symbols[s] = _mmobj->mm_symbol_new(cstr);
  }
  return _symbols[s];
}

void VM::register_primitive(std::string name, prim_function_t fun) {
  _primitives[name] = fun;
}

prim_function_t VM::get_primitive(std::string name) {
  assert(_primitives.find(name) != _primitives.end());
  return _primitives[name];
}
