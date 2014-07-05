#include "report.hpp"
#include <iostream>

using namespace std;

void bail(const string& msg) {
  cout << msg << "\n";
  exit(1);
}

ostream& debug() {
  return cout;
}
