#ifndef CORE_LOADER_HPP
#define CORE_LOADER_HPP

#include <list>
#include <string>
#include <map>
#include "defs.hpp"

class CoreImage {
  static word HEADER_SIZE;
  static const char* PRIMES_NAMES[];
  static int TOTAL_PRIMES;

public:
  CoreImage(const char*);
  // oop oop_entry();
  // oop oop_imodule();
  void load();
private:
  void load_header();
  void load_prime_objects_table();
  void relocate_addresses();
  bool is_prime(const char*);
  void read_file();
  word unpack_word(const char* data, int offset);
  void write_word(char* data, word target, word value);


  const char* _filepath;
  long _data_size;
  char* _data;

  //header
  word _num_entries;
  word _names_size;
  word _ot_size;

  std::map<std::string, oop> _primes;
};

#endif
