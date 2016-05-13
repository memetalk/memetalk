#include "log.hpp"
#include <stdlib.h>

std::string MMLog::red = "\x1b[31m";
std::string MMLog::green = "\x1b[32m";
std::string MMLog:: yellow = "\x1b[33m";
std::string MMLog:: blue = "\x1b[34m";
std::string MMLog:: magenta = "\x1b[35m";
std::string MMLog:: cyan = "\x1b[36m";
std::string MMLog:: bold = "\x1b[1m";
std::string MMLog:: underline = "\x1b[4m";
std::string MMLog:: normal = "\x1b(B\x1b[m";

MMLog MMLog::error() {
  return MMLog(LOG_ERROR);
}

MMLog MMLog::warning() {
  return MMLog(LOG_WARNING);
}

MMLog::MMLog(std::string feature, std::string prefix) : _running(false) {
  const char* loglevel_ = getenv("LOGLEVEL");
  std::string loglevel = loglevel_ ? loglevel_ : "";

  _enabled = (feature == LOG_WARNING) || //MMLog(LOG_WARNING)
    (feature == LOG_ERROR) ||            //MMLog(LOG_ERROR)
    (feature == LOG_ALL) ||              //MMLog(LOG_ALL)
    (loglevel == LOG_ALL) ||             //LOGLEVEL=al ./sf-vm
    loglevel.find(feature) != std::string::npos; //LOGLEVEL=...xx... && MMLog(LOG_XX)

  if (feature == LOG_WARNING) {
    _prefix = bold + red + underline + "WARNING: ";
  } else if (feature == LOG_ERROR) {
    _prefix = red + underline + "ERROR: ";
  } else {
    _prefix = prefix;
  }
}

MMLog& MMLog::operator<<(STDEndl msg) {
  if (_enabled) {
    std::cerr << normal << msg;
  }
  _running = false;
  return *this;
}
