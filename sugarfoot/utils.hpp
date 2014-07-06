#ifndef UTILS_HPP
#define UTILS_HPP

#include "defs.hpp"

char* read_file(const char* filepath, int* file_size);
word unpack_word(const char* data, int offset);
void relocate_addresses(char* data, int data_size, int start_reloc_table);

#endif
