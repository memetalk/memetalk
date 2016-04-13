#include "defs.hpp"
#include "utils.hpp"
#include "mmc_fun.hpp"
#include "core_image.hpp"
#include "mmobj.hpp"
#include "vm.hpp"
#include "process.hpp"

using namespace std;

word MMCFunction::HEADER_SIZE = 6 * WSIZE;
word MMCFunction::MAGIC_NUMBER = 0x420;

MMCFunction::MMCFunction(VM* vm, CoreImage* core_image, char* data, int data_size)
  : _log(LOG_MMCFUN), _vm(vm), _mmobj(vm->mmobj()), _core_image(core_image), _data_size(data_size), _data(data) {
}

void MMCFunction::load_header() {
  word magic_number = unpack_word(_data, 0 * WSIZE);
  _ot_size = unpack_word(_data,  1 * WSIZE);
  _er_size = unpack_word(_data, 2 * WSIZE);
  _es_size = unpack_word(_data, 3 * WSIZE);
  _names_size = unpack_word(_data,  4 * WSIZE);
  _cfun_addr = unpack_word(_data,  5 * WSIZE);

  _log << " ============ eval: ===========" << endl;
  _log << "Header:magic: " << magic_number << " =?= " << MMCFunction::MAGIC_NUMBER << endl;
  _log << "Header:ot_size: " << _ot_size << endl;
  _log << "Header:er_size: " << _er_size << endl;
  _log << "Header:es_size: " << _es_size << endl;
  _log << "Header:names_size: " << _names_size << endl;
  _log << "Header:cfun_addr: " << _cfun_addr << endl;
}

void MMCFunction::link_external_references() {
  const char* base = _data;
  int start_external_refs = HEADER_SIZE + _names_size + _ot_size;

  for (word i = 0; i < _er_size; i += (2 * WSIZE)) {
    word name_offset = unpack_word(_data, start_external_refs + i);
    char* name = (char*) (base + name_offset);
    word obj_offset = unpack_word(_data, start_external_refs + i + WSIZE);
    // word* obj = (word*) (base + obj_offset);
    // _log << obj_offset << " - " << *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
    * (word*) (base + obj_offset) = (word) _core_image->get_prime(name);
    // _log << "External refs " << obj_offset << " - " << (oop) *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
  }
}

oop MMCFunction::load(Process* proc /*, oop cmod*/) {
  load_header();
  relocate_addresses(_data, _data_size, HEADER_SIZE + _names_size + _ot_size + _er_size + _es_size);
  link_external_references();
  link_symbols(_data, _es_size, HEADER_SIZE + _names_size + _ot_size + _er_size, _vm, _core_image);

  oop cfun = (oop) & (_data[_cfun_addr]);
  _log << "New cfun outer: " << _mmobj->mm_compiled_function_outer_cfun(proc, cfun)
          << " is_top: " << _mmobj->mm_compiled_function_is_top_level(proc, cfun)
          << " env_size: " << _mmobj->mm_compiled_function_get_num_locals_or_env(proc, cfun)
          << " env_offset: " << _mmobj->mm_compiled_function_get_env_offset(proc, cfun) << endl;
  //TODO:
  // _mmobj->mm_compiled_function_set_owner(cmod);
  return cfun;
}
