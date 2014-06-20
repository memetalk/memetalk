#include "report.hpp"

void bail(const string& msg) {
  cout << msg << "\n";
  exit(1);
}

ostream& debug() {
  return cout;
}
