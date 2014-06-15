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

static int unpack_word(const string& data, int offset) {
  assert((offset+3) < data.size());
  int d = (int) (unsigned char) data[offset];
  d += (data[offset+1] << 8) + (data[offset+2] << 12) + (data[offset+3] << 16);
  return (long) d;
}


static string read_file(const char* filepath) {
  fstream file;
  file.open (filepath, fstream::in | fstream::binary);

  if (!file.is_open()) {
    bail(s("file not found: " + s(filepath)));
  }

  string contents(static_cast<stringstream const&>(stringstream() << file.rdbuf()).str());
  long size = file.tellg();

  debug() << "file size: "<< size << endl;

  file.close();
  return contents;
}

CoreImage::CoreImage(const string& data)
  : _data(data) {
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
  _num_entries = unpack_word(_data, 0);
  _names_size = unpack_word(_data, WSIZE);
  _ot_size = unpack_word(_data, WSIZE * 2);
  debug() << "Header:entries: " << _num_entries << endl;
  debug() << "Header:names_size: " << _names_size << endl;
  debug() << "Header:ot_size: " << _ot_size << endl;
}

void CoreImage::load_prime_objects_table() {
  int start_index = HEADER_SIZE + _names_size;
  const char* base = _data.c_str();

  for (int i = 0; i < _num_entries*2; i+=2) {
    int name_offset = unpack_word(_data, start_index + (i * WSIZE));
    char* prime_name = (char*) (base + name_offset);
    if (is_prime(prime_name)) {
      int obj_offset = unpack_word(_data, start_index + ((i+1) * WSIZE));
      debug() << "found prime " << prime_name << ":" << obj_offset << endl;
      oop prime_oop = (char*) (base + obj_offset);
      _primes[prime_name] = prime_oop;
    }
  }
}

void CoreImage::load() {
  load_header();
  load_prime_objects_table();
}

int main(int argc, char** argv) {
  if (argc != 2) {
    bail("usage: sf-vm <coreimage>");
  }
  string contents = read_file(argv[1]);
  CoreImage c = CoreImage(contents);
  c.load();

  return 0;
}
