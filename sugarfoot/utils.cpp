#include "utils.hpp"
#include "report.hpp"
#include <fstream>
#include <sstream>

using namespace std;

char* read_file(const char* filepath, int* file_size) {
  fstream file;
  file.open (filepath, fstream::in | fstream::binary);

  if (!file.is_open()) {
    bail(string("file not found: ") + filepath);
  }

  string contents(static_cast<stringstream const&>(stringstream() << file.rdbuf()).str());
  *file_size = file.tellg();

  debug() << "file size: "<< *file_size << endl;

  file.close();
  const char* str = contents.c_str();
  char* ret = (char*) malloc(sizeof(char) * *file_size);
  return (char*) memcpy(ret, str, sizeof(char) * *file_size);
}

word unpack_word(const char* data, int offset) {
  // assert((offset+WSIZE-1) < _data_size);
  return *((word*) &(data[offset]));
}

void write_word(char* data, word target, word value) {
  word* d = (word*) &(data[target]);
  *d = value;
}