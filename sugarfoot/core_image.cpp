#include <stdlib.h>
#include "core_image.hpp"
#include "report.hpp"
#include "utils.hpp"
#include "vm.hpp"

using namespace std;


word CoreImage::HEADER_SIZE = 4 * WSIZE;


const char* CoreImage::PRIMES_NAMES[] = {"Behavior",
                                        "Object_Behavior",
                                        "Object",
                                        "CompiledClass",
                                        "CompiledModule",
                                        "String",
                                        "Symbol",
                                        "Dictionary",
                                        "List",
                                        "Number",
                                        "CompiledFunction",
                                        "Function",
                                         "Context",
                                         "Boolean",
                                         "Exception",
                                         "ImportError",
                                         "DoesNotUnderstand",
                                         "ArityError",
                                         "CompileError",
                                         "Process",
                                         "Null"};
int CoreImage::TOTAL_PRIMES = 20;


CoreImage::CoreImage(VM* vm, const char* filepath)
  : _vm(vm), _filepath(filepath) {
}


bool CoreImage::is_prime(const char* name) {
  for (int i = 0; i < TOTAL_PRIMES; i++) {
    // debug() << name << " =?= " <<  PRIMES_NAMES[i] << endl;
    if (strcmp(name, PRIMES_NAMES[i]) == 0) {
      return true;
    }
  }
  return false;
}

//temporary workaround used on mmc_loader to get superclass
bool CoreImage::has_class(const char* name) {
  return (strcmp(name, "Behavior") != 0) &&
    (strcmp(name, "Object_Behavior") != 0) &&
    is_prime(name);
}


bool CoreImage::is_core_instance(const char* name) {
  return strcmp(name, "@core_module") == 0;
}

void CoreImage::load_header() {
  _num_entries = unpack_word(_data, 0 * WSIZE);
  _names_size = unpack_word(_data, 1 * WSIZE);
  _es_size = unpack_word(_data,  2 * WSIZE);
  _ot_size = unpack_word(_data,  3 * WSIZE);
  debug() << "Header:entries: " << _num_entries << endl;
  debug() << "Header:names_size: " << _names_size << endl;
  debug() << "Header:es_size: " << _es_size << endl;
  debug() << "Header:ot_size: " << _ot_size << endl;
}

void CoreImage::load_prime_objects_table() {
  int start_index = HEADER_SIZE + _names_size;
  const char* base = _data;

  for (int i = 0; i < _num_entries * 2; i += 2) {
    word name_offset = unpack_word(_data, start_index + (i * WSIZE));
    char* prime_name = (char*) (base + name_offset);
    if (is_prime(prime_name)) {
      word obj_offset = unpack_word(_data, start_index + ((i+1) * WSIZE));
      oop prime_oop = (oop) (base + obj_offset);
      _primes[prime_name] = prime_oop;
      debug() << "found prime " << prime_name << ":" << (oop) obj_offset << " (" << _primes[prime_name] << ")" << endl;
    } else if (is_core_instance(prime_name)) {
      word obj_offset = unpack_word(_data, start_index + ((i+1) * WSIZE));
      oop prime_oop = (oop) (base + obj_offset);
      _core_imod = prime_oop;
      debug() << "found core instance " << prime_name << ":" << (oop) obj_offset << " (" << _core_imod << ")" << endl;

    }
  }
}

oop CoreImage::get_prime(const char* name) {
  // debug() << "getting prime [" << name << "]" << endl;
  return _primes.at(name);
}

oop CoreImage::get_module_instance() {
  return _core_imod;
}

void CoreImage::load() {
  _data = read_mmc_file(_filepath, &_data_size);
  load_header();
  load_prime_objects_table();

  word index_size = _num_entries * 2 * WSIZE;
  relocate_addresses(_data, _data_size, HEADER_SIZE + _names_size + index_size + _ot_size + _es_size);
  link_symbols(_data, _es_size, HEADER_SIZE + _names_size + index_size + _ot_size, _vm, this);
}
