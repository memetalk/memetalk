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
  void load();
  oop get_prime(const char*);
private:
  void load_header();
  void load_prime_objects_table();
  bool is_prime(const char*);


  const char* _filepath;
  int _data_size;
  char* _data;

  //header
  word _num_entries;
  word _names_size;
  word _ot_size;

  std::map<std::string, oop> _primes;
};

#endif
