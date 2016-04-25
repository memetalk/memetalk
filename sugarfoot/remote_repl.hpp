#ifndef REMOTE_REPL_HPP
#define REMOTE_REPL_HPP

#include "process.hpp"
#include <string>

class Repl {
public:
  Repl(Process *proc) : _proc(proc), _current_imod(MM_NULL) {};
  void start_read_eval_loop();
private:
  char* readline(int client_sock);
  void dispatch(std::string msg);

  Process* _proc;
  oop _current_imod;
};

#endif
