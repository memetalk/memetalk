#include "vm.hpp"
#include "process.hpp"
#include "core_image.hpp"
#include "mmc_image.hpp"
#include "defs.hpp"
#include "mmobj.hpp"
#include "prims.hpp"
#include "report.hpp"
#include <assert.h>
#include "utils.hpp"

VM::VM(const char* core_img_filepath)
  : _core_image(new CoreImage(this, core_img_filepath)), _mmobj(new MMObj(this, _core_image)) {
}

MMObj* VM::mmobj() {
  return _mmobj;
}

void VM::dictionary_dump(oop dict) {
  number size = _mmobj->mm_dictionary_size(dict);

  debug() << "  dict vt: " << _mmobj->mm_object_vt(dict) << endl;
  debug() << "  dict size: " << size << endl;
  for (int i = 3; i < (3 + (size*2)); i++) {
    debug() << "  [" << i << "]: & " << &((oop*)dict)[i];
    debug() << endl;
    // debug() << " vt:" << * ((oop**)dict)[i]  << endl;
  }
}

void VM::dump_prime_info() {
  std::map<std::string, oop> primes = _core_image->get_primes();
  std::map<std::string, oop>::iterator it;
  for (it = primes.begin(); it != primes.end(); it++) {
    if (it->first == "Behavior" || it->first == "Object_Behavior") continue;
    oop klass = it->second;
    debug() << "PRIME " << klass << " name: " << _mmobj->mm_string_cstr(_mmobj->mm_class_name(klass)) << " vt: "
            << _mmobj->mm_object_vt(klass) << " dict:" << _mmobj->mm_class_dict(klass) << endl;
    dictionary_dump(_mmobj->mm_class_dict(klass));
  }
}

int VM::start(char* filepath) {
  _core_image->load();
  // dump_prime_info();

  oop imod = instantiate_module(filepath, MM_NULL);

  init_primitives(this);

  _process = new Process(this);
  oop retval = _process->run(imod, new_symbol("main"));
  oop retval_str = _process->do_send(retval, new_symbol("toString"));
  debug() << "RETVAL: " << _mmobj->mm_string_cstr(retval_str) << endl;
  return 0;
}

oop VM::new_symbol(const char* cstr) {
  std::string s = cstr;
  if (_symbols.find(s) == _symbols.end()) {
    _symbols[s] = _mmobj->mm_symbol_new(cstr);
    debug() << "new_symbol: Creating new symbol " << cstr << " = " << _symbols[s] << endl;
  } else {
    debug() << "new_symbol: returning existing symbol " << cstr << " = " << _symbols[s] << endl;
  }
  return _symbols[s];
}

oop VM::new_symbol(oop str) {
  assert(_mmobj->mm_object_vt(str) == _core_image->get_prime("String"));
  return new_symbol(_mmobj->mm_string_cstr(str));
}

void VM::register_primitive(std::string name, prim_function_t fun) {
  _primitives[name] = fun;
}

prim_function_t VM::get_primitive(std::string name) {
  assert(_primitives.find(name) != _primitives.end());
  return _primitives[name];
}

oop VM::instantiate_module(char* name_or_path, oop module_args_list) {
  MMCImage* mmc = new MMCImage(this, _core_image, name_or_path);
  mmc->load();
  return mmc->instantiate_module(module_args_list);
}

oop VM::get_prime(const char* name) {
  return _core_image->get_prime(name);
}
