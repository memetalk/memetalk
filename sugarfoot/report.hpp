#ifndef REPORT_HPP
#define REPORT_HPP

#include <iostream>
#include <string>
#include <fstream>

using std::endl;

void bail(const std::string& msg);
void done();
std::ostream& debug();

#endif
