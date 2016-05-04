#include "remote_repl.hpp"
#include "vm.hpp"
#include "process.hpp"
#include "mmobj.hpp"
#include <netdb.h>
#include <iostream>
#include <string>
#include <sstream>
#include <assert.h>
#include <stdlib.h>
#include <stdexcept>


using namespace std;

#define DBG() _log << _log.yellow + _log.bold + "[repl|" << __FUNCTION__ << "] " << _log.normal

#define ERROR(vm, clientsock, msg)  shutdown(clientsock, SHUT_RDWR);     \
                                    close(clientsock); \
                                    vm->bail(msg);

#define REPL_MESSAGE_OK "* OK\n"
#define REPL_MESSAGE_ERROR "* ERROR\n"

Repl::Repl(Process *proc)
  : _log(LOG_REPL), _proc(proc), _current_imod(MM_NULL) {
}

char* Repl::readmessage(int client_sock) {
  char message[1024];
  char buffer[1];
  int idx = 0;
  while (1) {
    if (recv(client_sock, buffer, 1, 0) < 0) {
      ERROR(_proc->vm(), client_sock, "recv error\n");
    } else if (buffer[0] == 0) {
      message[idx++] = '\0';
      return strdup(message);
    } else {
      message[idx++] = buffer[0];
    }
    if (idx > 1024) {
      ERROR(_proc->vm(), client_sock, "readmessage reading more than 1024 chars\n");
    }
  }
  ERROR(_proc->vm(), client_sock, "readmessage post while\n");
}

int Repl::compile_module(std::string module_name) {
  char* mmpath = getenv("MEME_PATH");
  std::stringstream s;
  s << "python -m pycompiler.compiler  "<< mmpath << module_name << ".mm";
  std::cerr << "Executing ... " << s.str() << std::endl;
  return system(s.str().c_str());
}

void Repl::load_module(int socket, char* module_name) {
    DBG() << "loading module: " << module_name << endl;

  if (compile_module(module_name) != 0) {
      write(socket, REPL_MESSAGE_ERROR, strlen(REPL_MESSAGE_ERROR));
  } else {
    try {
      _current_imod = _proc->vm()->instantiate_module(_proc, module_name, _proc->mmobj()->mm_list_new());
      write(socket, REPL_MESSAGE_OK, strlen(REPL_MESSAGE_OK));
    } catch(mm_exception_rewind e) {
      assert(0); //not sure what makes us reach this place
      // //TODO: it doesn't seem to reach here, it dies in process I think
      // write(socket, REPL_MESSAGE_ERROR, strlen(REPL_MESSAGE_ERROR));
      // _proc->vm()->print_error(_proc, e.mm_exception);
    } catch(const std::invalid_argument& e) {
      DBG() << "load failed" << endl;
      write(socket, REPL_MESSAGE_ERROR, strlen(REPL_MESSAGE_ERROR));
      //std::cerr << "Uncaugh C++ exception: " <<  e.what() << std::endl;
    }
  }
}

void Repl::doit(int socket, char* code) {

}

void Repl::dispatch(int socket, std::string msg) {
  if (msg.find("load") == 0) {
    char* module_name = strdup(msg.substr(5).c_str()); //strlen("load") == 4
    load_module(socket, module_name);
  } else if (msg.find("do-it") == 0) {
    char* code = strdup(msg.substr(5).c_str()); //strlen("do-it") == 5
    doit(socket, code);
  } else {
    cerr << "REPL: unknown command " << msg << endl;
  }
}

void Repl::start_read_eval_loop() {
  int port = 4200;
  int listen_fd;
  int client_sock;

  struct sockaddr_in servaddr;

  listen_fd = socket(AF_INET, SOCK_STREAM, 0);

  int on = 1;
  if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0) {
    _proc->vm()->bail("repl: setsockopt failed");
  }
  bzero( &servaddr, sizeof(servaddr));

  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = htons(INADDR_ANY);
  servaddr.sin_port = htons(port);

  if (bind(listen_fd, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0) {
    _proc->vm()->bail("repl: bind failed");
  }

  listen(listen_fd, 10);

  client_sock = accept(listen_fd, (struct sockaddr*) NULL, NULL);
  if (client_sock < 0) {
    _proc->vm()->bail("repl: accept failed");
  }

  // write(client_sock, REPL_MESSAGE_OK, strlen(REPL_MESSAGE_OK));

  char* msg;
  while (1) {
    msg = readmessage(client_sock);
    DBG() << "got msg: " << msg << endl;
    dispatch(client_sock, msg);
  }
}
