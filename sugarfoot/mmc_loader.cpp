#include "defs.hpp"
#include "utils.hpp"
#include "report.hpp"
#include "mmc_loader.hpp"

using namespace std;

word MMCImage::HEADER_SIZE = 4 * WSIZE;
word MMCImage::MAGIC_NUMBER = 0x420;

MMCImage::MMCImage(const char* filepath)
  : _filepath(filepath) {
}

void MMCImage::load_header() {
  word magic_number = unpack_word(_data, 0 * WSIZE);
  _names_size = unpack_word(_data, 1 * WSIZE);
  _ot_size = unpack_word(_data,  2 * WSIZE);
  _names_size = unpack_word(_data,  3 * WSIZE);

  debug() << "Header:magic: " << magic_number << " =?= " << MMCImage::MAGIC_NUMBER << endl;
  debug() << "Header:ot_size: " << _ot_size << endl;
  debug() << "Header:es_size: " << _es_size << endl;
  debug() << "Header:names_size: " << _names_size << endl;
}

void MMCImage::load() {
  _data = read_file(_filepath, &_data_size);
  load_header();
}
