#ifndef REMOTE_REPL_HPP
#define REMOTE_REPL_HPP

#include "process.hpp"
#include "log.hpp"
#include <string>

class Repl {
public:
  Repl(Process*);
  void start_read_eval_loop();
private:
  char* readmessage(int client_sock);
  void dispatch(int socket, std::string msg);

  void load_module(int socket, char* module_name);
  void doit(int socket, char* code);

  int compile_module(std::string module_name);

  MMLog _log;
  Process* _proc;
  oop _current_imod;
};

#endif
