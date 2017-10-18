/*
 * Copyright 2012-2017 Thiago Silva <thiago@memetalk.org>
 *
 * Licensed under the BSD-2 license. See LICENSE file in the project root for
 * full license information.
 */
#ifndef LOG_HPP
#define LOG_HPP

#include <iostream>

//log levels
#define LOG_ALL "al"

// debugging levels per module
#define LOG_VM  "vm"     //vm.cpp
#define LOG_CORE "co"    //core_image.cpp
#define LOG_MMOBJ "mo"   //mmobj.cpp
#define LOG_MECFUN "mf"  //mec_fun.cpp
#define LOG_MECIMG "mi"  //mec_image.cpp
#define LOG_PRIMS "pr"   //prims.cpp
#define LOG_QTPRIMS "qp" //qt_prims.cpp
#define LOG_CTRL "ct"    //ctrl.cpp
#define LOG_UTILS "ut"   //utils.cpp
#define LOG_REPL "rp"    //remote_repl.cpp
#define LOG_TARGET_PROC "tp" //process.cpp [main process]
#define LOG_DBG_PROC "dp"    //process.cpp [dbg process]

// debugging feature: registers
#define LOG_TARGET_PROC_REG "tr"
#define LOG_DBG_PROC_REG "dr"

// debugging feature: code
#define LOG_TARGET_PROC_BODY "tb"
#define LOG_DBG_PROC_BODY "db"

// debugging feature: stack
#define LOG_TARGET_PROC_STACK "ts"
#define LOG_DBG_PROC_STACK "ds"
#define LOG_TARGET_PROC_TRACE "tt"
#define LOG_DBG_PROC_TRACE "dt"

// standard templates
#define LOG_WARNING "wa"
#define LOG_ERROR "er"

using std::endl;

class MMLog {
private:
  bool _running;
  std::string _prefix;
public:
  bool _enabled;
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
