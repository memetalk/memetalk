#ifndef SF_LOADER_HPP
#define SF_LOADER_HPP

#include <list>
#include <string>
#include <map>

#define  WSIZE 8

using namespace std;

typedef void* oop;

#if WSIZE == 4
  typedef int word;
#elif WSIZE == 8
  typedef long word;
#endif

class CoreImage {
  static word HEADER_SIZE;
  static const char* PRIMES_NAMES[];
  static int TOTAL_PRIMES;

public:
  CoreImage(const char*
);
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
word CoreImage::HEADER_SIZE = 3 * WSIZE;

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
                                        "Context"};
int CoreImage::TOTAL_PRIMES = 13;


#endif
