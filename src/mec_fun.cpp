/*
 * Copyright 2012-2017 Thiago Silva <thiago@memetalk.org>
 *
 * Licensed under the BSD-2 license. See LICENSE file in the project root for
 * full license information.
 */
#include "defs.hpp"
#include "utils.hpp"
#include "mec_fun.hpp"
#include "core_image.hpp"
#include "mmobj.hpp"
#include "vm.hpp"
#include "process.hpp"

using namespace std;

#define DBG() _log << _log.blue + _log.bold + "[MECFun|" << __FUNCTION__ << "] " << _log.normal
#define WARNING() MMLog::warning() << "[MECFun|" << __FUNCTION__ << "] " << _log.normal
#define ERROR() MMLog::error() << "[MECFun|" << __FUNCTION__ << "] " << _log.normal

word MECFunction::HEADER_SIZE = 6 * WSIZE;
word MECFunction::MAGIC_NUMBER = 0x420;

MECFunction::MECFunction(VM* vm, CoreImage* core_image, char* data, int data_size)
  : _log(LOG_MECFUN), _vm(vm), _mmobj(vm->mmobj()), _core_image(core_image), _data_size(data_size), _data(data) {
}

void MECFunction::load_header() {
  word magic_number = unpack_word(_data, 0 * WSIZE);
  _ot_size = unpack_word(_data,  1 * WSIZE);
  _er_size = unpack_word(_data, 2 * WSIZE);
  _es_size = unpack_word(_data, 3 * WSIZE);
  _names_size = unpack_word(_data,  4 * WSIZE);
  _cfun_addr = unpack_word(_data,  5 * WSIZE);

  DBG() << " ============ eval: ===========" << endl;
  DBG() << "Header:magic: " << magic_number << " =?= " << MECFunction::MAGIC_NUMBER << endl;
  DBG() << "Header:ot_size: " << _ot_size << endl;
  DBG() << "Header:er_size: " << _er_size << endl;
  DBG() << "Header:es_size: " << _es_size << endl;
  DBG() << "Header:names_size: " << _names_size << endl;
  DBG() << "Header:cfun_addr: " << _cfun_addr << endl;
}

void MECFunction::link_external_references() {
  const char* base = _data;
  int start_external_refs = HEADER_SIZE + _names_size + _ot_size;

  for (word i = 0; i < _er_size; i += (2 * WSIZE)) {
    word name_offset = unpack_word(_data, start_external_refs + i);
    char* name = (char*) (base + name_offset);
    word obj_offset = unpack_word(_data, start_external_refs + i + WSIZE);
    // word* obj = (word*) (base + obj_offset);
    // DBG() << obj_offset << " - " << *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
    * (word*) (base + obj_offset) = (word) _core_image->get_prime(name);
    // DBG() << "External refs " << obj_offset << " - " << (oop) *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
  }
}

oop MECFunction::load(Process* proc /*, oop cmod*/) {
  load_header();
  relocate_addresses(_data, _data_size, HEADER_SIZE + _names_size + _ot_size + _er_size + _es_size);
  link_external_references();
  link_symbols(_data, _es_size, HEADER_SIZE + _names_size + _ot_size + _er_size, _vm, _core_image);

  oop cfun = (oop) & (_data[_cfun_addr]);
  DBG() << "New cfun outer: " << _mmobj->mm_compiled_function_outer_cfun(proc, cfun)
        << " is_top: " << CFUN_IS_TOP_LEVEL(_mmobj->mm_compiled_function_get_header(proc, cfun))
        << " env_size: " << CFUN_STORAGE_SIZE(_mmobj->mm_compiled_function_get_header(proc, cfun))
        << " env_offset: " << CFUN_ENV_OFFSET(_mmobj->mm_compiled_function_get_header(proc, cfun)) << endl;
  //TODO:
  // _mmobj->mm_compiled_function_set_owner(cmod);
  return cfun;
}
