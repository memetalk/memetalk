#include "report.hpp"
#include <iostream>

using namespace std;

void bail(const string& msg) {
  cerr << msg << "\n";
  done();
}

void done() {
  exit(1);
}

ostream& debug() {
  return cout;
}
