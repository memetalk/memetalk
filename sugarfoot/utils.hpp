#ifndef UTILS_HPP
#define UTILS_HPP

#include "defs.hpp"
#include <string>

class VM;
class Process;
class CoreImage;

char* read_mmc_file(const std::string& filepath, int* file_size);
word unpack_word(const char* data, int offset);
void relocate_addresses(char* data, int data_size, int start_reloc_table);
void link_symbols(char* data, int es_size, int start_external_symbols, VM* vm, CoreImage* core);

int decode_opcode(bytecode);
int decode_args(bytecode);


bool is_small_int(oop num);
number untag_small_int(oop num);
oop tag_small_int(number num);

bool check_and_print_exception(Process* proc, int exc, oop ex);

#endif
