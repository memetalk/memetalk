#include "remote_repl.hpp"
#include "vm.hpp"
#include "process.hpp"
#include "mmobj.hpp"
#include <netdb.h>
#include <iostream>
#include <string>
#include <assert.h>

using namespace std;

#define ERROR(vm, clientsock, msg)  shutdown(clientsock, SHUT_RDWR);     \
                                    close(clientsock); \
                                    vm->bail(msg);

#define REPL_MESSAGE_OK "* OK\n"
#define REPL_MESSAGE_ERROR "* ERROR\n"

char* Repl::readline(int client_sock) {
  char message[1024];
  char buffer[1];
  int idx = 0;
  while (1) {
    if (recv(client_sock, buffer, 1, 0) < 0) {
      ERROR(_proc->vm(), client_sock, "recv error\n");
    }
    if (buffer[0] == 13) {
      continue;
    } else if (buffer[0] == 10) {
      message[idx++] = '\0';
      return strdup(message);
    } else {
      message[idx++] = buffer[0];
    }
  }
  ERROR(_proc->vm(), client_sock, "readline post while\n");
}

void Repl::dispatch(int socket, std::string msg) {
  if (msg.find("load") == 0) {
    char* module_name = strdup(msg.substr(5).c_str()); //strlen("load") == 4
    try {
      _current_imod = _proc->vm()->instantiate_module(_proc, module_name, _proc->mmobj()->mm_list_new());
      write(socket, REPL_MESSAGE_OK, strlen(REPL_MESSAGE_OK));
    } catch(mm_exception_rewind e) {
      //TODO: it doesn't seem to reach here, it dies in process I think
      write(socket, REPL_MESSAGE_ERROR, strlen(REPL_MESSAGE_ERROR));
      _proc->vm()->print_error(_proc, e.mm_exception);
    }
  // } else if(msg.find()) {
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

  write(client_sock, REPL_MESSAGE_OK, strlen(REPL_MESSAGE_OK));

  char* msg;
  while (1) {
    msg = readline(client_sock);
    dispatch(client_sock, msg);
  }
}
