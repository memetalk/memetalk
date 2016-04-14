#ifndef LOG_HPP
#define LOG_HPP

#include <iostream>

//log levels
#define LOG_ALL "al"

// debugging levels per module
#define LOG_VM  "vm"
#define LOG_CORE "co"
#define LOG_LOADER "lo"
#define LOG_MMOBJ "mo"
#define LOG_MMCFUN "mf"
#define LOG_MMCIMG "mi"
#define LOG_PRIMS "pr"
#define LOG_QTPRIMS "qp"
#define LOG_CTRL "ct"
#define LOG_UTILS "ut"
#define LOG_TARGET_PROC "tp"
#define LOG_DBG_PROC "dp"
#define LOG_TARGET_MMT "mt"
#define LOG_DBG_MMT "md"

// debugging feature: registers
#define LOG_TARGET_PROC_REG "tr"
#define LOG_DBG_PROC_REG "dr"

// debugging feature: stack
#define LOG_TARGET_PROC_STACK "ts"
#define LOG_DBG_PROC_STACK "ds"

// standard templates
#define LOG_WARNING "wa"
#define LOG_ERROR "er"

using std::endl;

class MMLog {
private:
  bool _enabled;
  bool _running;
  std::string _prefix;
public:
  static std::string red;
  static std::string green;
  static std::string yellow;
  static std::string blue;
  static std::string magenta;
  static std::string cyan;
  static std::string bold;
  static std::string underline;
  static std::string normal;

  static MMLog error();
  static MMLog warning();

  MMLog(std::string feature, std::string prefix = "");

  // this is the type of std::cout
  typedef std::basic_ostream<char, std::char_traits<char> > STDcoutType;
  // this is the function signature of std::endl
  typedef STDcoutType& (*STDEndl)(STDcoutType&);

  MMLog& operator<<(STDEndl msg);

  template<typename T>
  MMLog& operator <<(const T& msg) {
    if (_enabled) {
      if (!_running) {
        std::cerr << _prefix;
        _running = true;
      }
      std::cerr << msg;
    }
    return *this;
  };
};
#endif
