#include "defs.hpp"
#include "utils.hpp"
#include "report.hpp"
#include "mmc_loader.hpp"
#include "core_loader.hpp"

using namespace std;

word MMCImage::HEADER_SIZE = 4 * WSIZE;
word MMCImage::MAGIC_NUMBER = 0x420;

MMCImage::MMCImage(CoreImage* core_image, const char* filepath)
  : _core_image(core_image), _filepath(filepath) {
}

void MMCImage::load_header() {
  word magic_number = unpack_word(_data, 0 * WSIZE);
  _ot_size = unpack_word(_data,  1 * WSIZE);
  _es_size = unpack_word(_data, 2 * WSIZE);
  _names_size = unpack_word(_data,  3 * WSIZE);

  debug() << "Header:magic: " << magic_number << " =?= " << MMCImage::MAGIC_NUMBER << endl;
  debug() << "Header:ot_size: " << _ot_size << endl;
  debug() << "Header:es_size: " << _es_size << endl;
  debug() << "Header:names_size: " << _names_size << endl;
}

void MMCImage::fix_external_references() {
  const char* base = _data;
  int start_external_symbols = HEADER_SIZE + _names_size + _ot_size;

  for (int i = 0; i < _es_size; i += (2 * WSIZE)) {
    word name_offset = unpack_word(_data, start_external_symbols + i);
    char* name = (char*) (base + name_offset);
    word obj_offset = unpack_word(_data, start_external_symbols + i + WSIZE);
    // word* obj = (word*) (base + obj_offset);
    // debug() << obj_offset << " - " << *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
    * (word*) (base + obj_offset) = (word) _core_image->get_prime(name);
    // debug() << obj_offset << " - " << *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
  }
}

void MMCImage::instantiate_module() {

}

void MMCImage::load() {
  _data = read_file(_filepath, &_data_size);
  load_header();
  relocate_addresses(_data, _data_size, HEADER_SIZE + _names_size + _ot_size + _es_size);
  fix_external_references();
  _compiled_module = (oop) & _data[HEADER_SIZE + _names_size];
}
