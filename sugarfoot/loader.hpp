#ifndef SF_LOADER_HPP
#define SF_LOADER_HPP

#include <list>
#include <string>
#include <map>

using namespace std;

typedef void* oop;

class CoreImage {
  static int HEADER_SIZE;
  static int WSIZE;
  static const char* PRIMES_NAMES[];
  static int TOTAL_PRIMES;

public:
  CoreImage(const std::string&);
  // oop oop_entry();
  // oop oop_imodule();
  void load();
private:
  void load_header();
  void load_prime_objects_table();
  bool is_prime(const char*);

  std::string _data;

  //header
  int _num_entries;
  int _names_size;
  int _ot_size;

  std::map<std::string, oop> _primes;
};
int CoreImage::WSIZE = 4;
int CoreImage::HEADER_SIZE = 3 * WSIZE;

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
