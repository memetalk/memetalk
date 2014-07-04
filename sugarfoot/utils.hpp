#ifndef UTILS_HPP
#define UTILS_HPP

#include "defs.hpp"

char* read_file(const char* filepath, int* file_size);
word unpack_word(const char* data, int offset);
void write_word(char* data, word target, word value);

#endif
