#include "report.hpp"
#include <iostream>
#include <stdlib.h>

using namespace std;

void bail(const string& msg) {
  cerr << msg << "\n";
  done();
}

void done() {
  exit(1);
}

static std::ostream null_stream(0);


ostream& debug() {
  if (getenv("DEBUG")) {
    return cerr;
  } else {
    return null_stream;
  }
}
