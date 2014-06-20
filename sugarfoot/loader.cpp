#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <string.h>
#include <sstream>
#include "loader.hpp"

#include <arpa/inet.h>
#include <assert.h>


#define s(x) (string(x))


static void bail(const string& msg) {
  cout << msg << "\n";
  exit(1);
}

static ostream& debug() {
  return cout;
}

CoreImage::CoreImage(const char* filepath)
  : _filepath(filepath) {
}

word CoreImage::unpack_word(const char* data, int offset) {
  assert((offset+WSIZE-1) < _data_size);
  return *((word*) &(data[offset]));
}

void CoreImage::write_word(char* data, word target, word value) {
  word* d = (word*) &(data[target]);
  *d = value;
}


void CoreImage::read_file() {
  fstream file;
  file.open (_filepath, fstream::in | fstream::binary);

  if (!file.is_open()) {
    bail(s("file not found: " + s(_filepath)));
  }

  string contents(static_cast<stringstream const&>(stringstream() << file.rdbuf()).str());
  _data_size = file.tellg();

  debug() << "file size: "<< _data_size << endl;

  file.close();
  _data = (char*) contents.c_str();
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

void CoreImage::load_header() {
  _num_entries = unpack_word(_data, 0 * WSIZE);
  _names_size = unpack_word(_data, 1 * WSIZE);
  _ot_size = unpack_word(_data,  2 * WSIZE);
  debug() << "Header:entries: " << _num_entries << endl;
  debug() << "Header:names_size: " << _names_size << endl;
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
      debug() << "found prime " << prime_name << ":" << obj_offset << endl;
      oop prime_oop = (char*) (base + obj_offset);
      _primes[prime_name] = prime_oop;
    }
  }
}

void CoreImage::relocate_addresses() {
  word index_size = _num_entries * 2 * WSIZE;
  int start_reloc_table = HEADER_SIZE + _names_size + index_size + _ot_size;

  const char* base = _data;

  for (int i = start_reloc_table; i < _data_size; i += WSIZE) {
    word target = unpack_word(_data,  i);
    word local_ptr = unpack_word(_data,  target);
    debug() << target << "-" << local_ptr << endl;
    write_word(_data, target, (long) (base + local_ptr));
  }
}

void CoreImage::load() {
  read_file();
  load_header();
  load_prime_objects_table();
  relocate_addresses();
}

int main(int argc, char** argv) {
  if (argc != 2) {
    bail("usage: sf-vm <coreimage>");
  }
  CoreImage c = CoreImage(argv[1]);
  c.load();

  return 0;
}
