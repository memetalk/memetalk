#include "utils.hpp"
#include "report.hpp"
#include <fstream>
#include <sstream>
#include "vm.hpp"
#include "core_image.hpp"

using namespace std;

static char* read_file(fstream& file, int* file_size) {
  string contents(static_cast<stringstream const&>(stringstream() << file.rdbuf()).str());
  *file_size = file.tellg();

  debug() << "file size: "<< *file_size << endl;

  file.close();
  const char* str = contents.c_str();
  char* ret = (char*) malloc(sizeof(char) * *file_size);
  return (char*) memcpy(ret, str, sizeof(char) * *file_size);
}

char* read_mmc_file(const std::string& name_or_path, int* file_size) {
  fstream file;

  std::string filepath = name_or_path;

  debug() << "read_mmc_file: " << filepath << endl;
  if (filepath[0] == '.' || filepath[0] == '/') { //relative or absolute path
    file.open(filepath.c_str(), fstream::in | fstream::binary);
  } else { //filename
    if (filepath.substr(filepath.find_last_of(".") + 1) != "img" &&
        filepath.substr(filepath.find_last_of(".") + 1) != "mmc") {
      filepath = filepath + ".mmc";
    }
    file.open(filepath.c_str(), fstream::in | fstream::binary);
    if (!file.good()) {
      char* mmpath = getenv("MEME_PATH");
      std::string base = mmpath == NULL ? "" : mmpath;
      debug() << "failed to open " << name_or_path <<
        " trying " << (base + filepath) << endl;
      file.open((base + filepath).c_str(), fstream::in | fstream::binary);
      if (!file.good()) {
        bail("Unable to open file");
      }
    }
  }

  if (!file.is_open()) {
    bail(string("file not found: ") + filepath);
  }
  return read_file(file, file_size);
}


word unpack_word(const char* data, int offset) {
  // assert((offset+WSIZE-1) < _data_size);
  return *((word*) &(data[offset]));
}

void relocate_addresses(char* data, int data_size, int start_reloc_table) {
  const char* base = data;

  for (int i = start_reloc_table; i < data_size; i += WSIZE) {
    word target = unpack_word(data,  i);
    word local_ptr = unpack_word(data,  target);
    // debug() << (oop) target << "->" << (oop) (base + local_ptr) << endl;
    // write_word(data, target, (word) (base + local_ptr));
    * (word*) &(data[target]) = (word) (base + local_ptr);
  }
}


void link_symbols(char* data, int es_size, int start_external_symbols, VM* vm, CoreImage*) {
  const char* base = data;

  for (int i = 0; i < es_size; i += (2 * WSIZE)) {
    word name_offset = unpack_word(data, start_external_symbols + i);
    char* name = (char*) (base + name_offset);
    word obj_offset = unpack_word(data, start_external_symbols + i + WSIZE);
    word* obj = (word*) (base + obj_offset);
    debug() << "Symbol: " << (oop) obj_offset << " - " << (oop) *obj << " [" << name << "] " << endl;
    * obj = (word) vm->new_symbol(name);
    // debug() << "offset: " << (oop) obj_offset << " - obj: " << (oop) *obj
    //         << " [" << name << "] -> " << " vt: " << * (oop*) *obj << " == " << core->get_prime("Symbol") << endl;
  }
}

int decode_opcode(bytecode code) {
  return (0xFF000000 & code) >> 24;
}

int decode_args(bytecode code) {
  return 0xFFFFFF & code;
}



bool is_small_int(oop num) {
#if WSIZE == 8
  return (bool) ((word) num & 0x8000000000000000);
#else
  return (bool) ((word) num & 0x80000000);
#endif
}

number untag_small_int(oop num) {
  word w = (word) num;
#if WSIZE == 8
  if (w & 0x4000000000000000) { //Negative
    return (w & 0x7FFFFFFFFFFFFFFF) | 0x8000000000000000;
  } else {
    return w & 0x7FFFFFFFFFFFFFFF;
  }
#else
  if (num & 0x40000000) { //Negative
    return (w & 0x7FFFFFFF) | 0x80000000;
  } else {
    return (w & 0x7FFFFFFF;
  }
#endif
}

number tag_small_int(oop num) {
#if WSIZE == 8
  return ((word) num) | 0x8000000000000000;
#else
  return ((word) num) | 0x80000000;
#endif
}
