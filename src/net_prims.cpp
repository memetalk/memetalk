/*
 * Copyright 2012-2017 Thiago Silva <thiago@memetalk.org>
 *
 * Licensed under the BSD-2 license. See LICENSE file in the project root for
 * full license information.
 */
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#include <iostream>
#include "process.hpp"
#include "vm.hpp"

#define TYPE_CHECK(o, t, e)                                             \
  if (!(proc->mmobj()->mm_object_vt(o) == proc->vm()->get_prime(t))) {  \
    proc->raise("TypeError", e);                                        \
  }

#define TYPE_CHECK_ARG(n, t)                                            \
  TYPE_CHECK(proc->get_arg(n), t, "Argument " #n " Expected " t)

static int dict_maybe_get(Process* proc, const char* symbol, oop dict) {
  oop key = proc->mmobj()->mm_symbol_new(symbol);
  if (proc->mmobj()->mm_dictionary_has_key(proc, dict, key)) {
    return untag_small_int(proc->mmobj()->mm_dictionary_get(proc, dict, key));
  }
  return 0;
}

static void setup_addrinfo_with_dict(Process* proc, addrinfo* hints, oop hints_oop) {
  memset(hints, 0, sizeof(struct addrinfo));
  hints->ai_flags = dict_maybe_get(proc, "ai_flags", hints_oop);
  hints->ai_family = dict_maybe_get(proc, "ai_family", hints_oop);
  hints->ai_socktype = dict_maybe_get(proc, "ai_socktype", hints_oop);
  hints->ai_protocol = dict_maybe_get(proc, "ai_protocol", hints_oop);
}

static void free_addrinfo(GC_PTR addrinfo_oop, void* _proc) {
  Process* proc = (Process*) _proc;
  // std::cerr << " %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% free_addr_info " << addrinfo_oop << " " << proc << endl;
  struct addrinfo *addr = (struct addrinfo *) ((oop*)addrinfo_oop)[2];
  freeaddrinfo(addr);
}

static oop addrinfo_new(Process* proc, addrinfo *addr, bool finalize, int *exc) {
  oop AddrInfo = proc->send_0(proc->mp(), proc->vm()->new_symbol("AddrInfo"), exc);
  if (*exc != 0) {
    return AddrInfo;
  }
  oop res =  proc->send_1(AddrInfo, proc->vm()->new_symbol("new"), (oop)addr, exc);
  if (finalize) {
    //TODO: test & make sure this works
    // std::cerr << "setting up finalizer for " << res << endl;
    void* old_env;
    GC_finalization_proc old_finalizer;
    GC_register_finalizer(res, free_addrinfo, proc, &old_finalizer, &old_env);
  }
  return res;
}

static int prim_net_getaddrinfo(Process* proc) {
  oop node_oop = proc->get_arg(0);
  oop service_oop = proc->get_arg(1);
  oop hints_oop = proc->get_arg(2);

  struct addrinfo hints;
  setup_addrinfo_with_dict(proc, &hints, hints_oop);

  const char* host = (node_oop == MM_NULL ? NULL :
                      proc->mmobj()->mm_string_cstr(proc, node_oop));
  const char* service = (service_oop == MM_NULL ? NULL :
                         proc->mmobj()->mm_string_cstr(proc, service_oop));

  int s;
  oop output = proc->mmobj()->mm_list_new();
  struct addrinfo *result, *rp;
  if ((s = getaddrinfo(host, service, &hints, &result)) != 0) {
    proc->raise_local("NetworkError", gai_strerror(s));
    return PRIM_RAISED;
  } else {
    int i;
    for (i = 0, rp = result; rp != NULL; rp = rp->ai_next, i++) {
      int exc;
      oop addrinfo_oop = addrinfo_new(proc, rp, i == 0, &exc);
      if (exc != 0) {
        proc->stack_push(addrinfo_oop);
        return PRIM_RAISED;
      }
      proc->mmobj()->mm_list_append(proc, output, addrinfo_oop);
    }
  }
  proc->stack_push(output);
  return 0;
}

static oop sockaddr_new(Process* proc, sockaddr *addr, socklen_t len, int *exc) {
  oop SockAddr = proc->send_0(proc->mp(), proc->vm()->new_symbol("SockAddr"), exc);
  if (*exc != 0) {
    return SockAddr;
  }
  return proc->send_2(SockAddr, proc->vm()->new_symbol("new"), (oop)addr, (oop) len, exc);
}

static int prim_net_socket(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int domain = untag_small_int(proc->get_arg(0));
  TYPE_CHECK_ARG(1, "Integer");
  int type = untag_small_int(proc->get_arg(1));
  TYPE_CHECK_ARG(2, "Integer");
  int protocol = untag_small_int(proc->get_arg(2));
  int ret = socket(domain, type, protocol);
  if (ret == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  }
  proc->stack_push(tag_small_int(ret));
  return 0;
}

static int prim_net_setsockopt(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int sockfd = untag_small_int(proc->get_arg(0));
  TYPE_CHECK_ARG(1, "Integer");
  int level = untag_small_int(proc->get_arg(1));
  TYPE_CHECK_ARG(2, "Integer");
  int optname = untag_small_int(proc->get_arg(2));
  TYPE_CHECK_ARG(3, "Integer");
  int optval = untag_small_int(proc->get_arg(3));

  // TODO: Only int flags are supported for now, which means that
  // SO_LINGER can't really be used. Maybe instead of just assuming
  // that `optval` is an integer, it could accept instancess of the
  // class "LingerOptions".
  int result = setsockopt(sockfd, level, optname, &optval, sizeof(optval));
  if (result == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  }
  proc->stack_push(tag_small_int(result));
  return 0;
}

static int prim_net_bind(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int sockfd = untag_small_int(proc->get_arg(0));
  //TYPE_CHECK_ARG(1, "Object");
  oop addrinfo_oop = proc->get_arg(1);
  struct addrinfo* addrinfo = (struct addrinfo *) ((oop *) addrinfo_oop)[2];
  int result = bind(sockfd, addrinfo->ai_addr, addrinfo->ai_addrlen);
  if (result == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  }
  proc->stack_push(tag_small_int(result));
  return 0;
}

static int prim_net_listen(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int sockfd = untag_small_int(proc->get_arg(0));
  TYPE_CHECK_ARG(1, "Integer");
  int backlog = untag_small_int(proc->get_arg(1));
  int result = listen(sockfd, backlog);
  if (result == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  } else {
    proc->stack_push(tag_small_int(result));
    return 0;
  }
}

static int prim_net_accept(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int sockfd = untag_small_int(proc->get_arg(0));

  //TODO: test & make sure this is freed
  struct sockaddr_storage* addr = (struct sockaddr_storage*) GC_MALLOC(sizeof(struct sockaddr_storage));
  socklen_t addrlen = sizeof(struct sockaddr_storage);
  int fd = accept(sockfd, (struct sockaddr *) addr, &addrlen);
  if (fd == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  }

  int exc;
  oop sockaddr_oop = sockaddr_new(proc, (struct sockaddr*)addr, addrlen, &exc);
  if (exc != 0) {
    proc->stack_push(sockaddr_oop);
    return PRIM_RAISED;
  }
  oop res = proc->mmobj()->mm_list_new();
  proc->mmobj()->mm_list_append(proc, res, tag_small_int(fd));
  proc->mmobj()->mm_list_append(proc, res, sockaddr_oop);
  proc->stack_push(res);
  return 0;
}

static int prim_net_addrinfo_ai_family(Process* proc) {
  oop dself = proc->dp();
  struct addrinfo *addr = (struct addrinfo *) ((oop*)dself)[2];
  proc->stack_push(tag_small_int(addr->ai_family));
  return 0;
}

static int prim_net_addrinfo_ai_socktype(Process* proc) {
  oop dself = proc->dp();
  struct addrinfo *addr = (struct addrinfo *) ((oop*)dself)[2];
  proc->stack_push(tag_small_int(addr->ai_socktype));
  return 0;
}

static int prim_net_addrinfo_ai_protocol(Process* proc) {
  oop dself = proc->dp();
  struct addrinfo *addr = (struct addrinfo *) ((oop*)dself)[2];
  proc->stack_push(tag_small_int(addr->ai_protocol));
  return 0;
}

static int prim_net_close(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int fd = untag_small_int(proc->get_arg(0));

  int ret = close(fd);
  if (ret == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  }
  proc->stack_push(tag_small_int(ret));
  return 0;
}

static int prim_net_addrinfo_ai_addr(Process* proc) {
  oop dself = proc->dp();
  struct addrinfo *addr = (struct addrinfo *) ((oop*)dself)[2];
  int exc;
  oop saddr = sockaddr_new(proc, addr->ai_addr, addr->ai_addrlen, &exc);
  proc->stack_push(saddr);
  return exc == 1;
}



static int prim_net_recv(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int sockfd = untag_small_int(proc->get_arg(0));
  TYPE_CHECK_ARG(1, "Integer");
  size_t len = untag_small_int(proc->get_arg(1));
  TYPE_CHECK_ARG(2, "Integer");
  int flags = untag_small_int(proc->get_arg(2));
  char buf[len+1];
  ssize_t bytes_read = recv(sockfd, &buf, len, flags);
  if (bytes_read == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  }
  buf[bytes_read] = '\0';
  proc->stack_push(proc->mmobj()->mm_string_new(buf));
  return 0;
}

static int prim_net_send(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int sockfd = untag_small_int(proc->get_arg(0));
  TYPE_CHECK_ARG(1, "String");
  oop buf_oop = proc->get_arg(1);
  TYPE_CHECK_ARG(2, "Integer");
  int flags = untag_small_int(proc->get_arg(2));

  size_t len = proc->mmobj()->mm_string_size(proc, buf_oop);
  const char *buf = proc->mmobj()->mm_string_cstr(proc, buf_oop);
  ssize_t result = send(sockfd, (const void *) buf, len, flags);
  if (result == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  }
  proc->stack_push(tag_small_int(result));
  return 0;
}

static int prim_net_connect(Process* proc) {
  TYPE_CHECK_ARG(0, "Integer");
  int sockfd = untag_small_int(proc->get_arg(0));
  oop sockaddr_oop = proc->get_arg(1);

  sockaddr *addr = (struct sockaddr *) ((oop*)sockaddr_oop)[2];
  //TODO: sure sizeof(socketlen_t) is always sizeof(int)?
  socklen_t len = untag_small_int(((oop*)sockaddr_oop)[3]);

  int res = connect(sockfd, addr, len);
  if (res == -1) {
    proc->raise_local("NetworkError", strerror(errno));
    return PRIM_RAISED;
  }
  proc->stack_push(tag_small_int(res));
  return 0;
}



// static oop create_addrinfo(Process* proc, struct addrinfo* addrinfo) {
//   int exc;
//   oop imod = proc->mp();
//   oop mmclass = proc->send_0(imod, proc->vm()->new_symbol("AddrInfo"), &exc);
//   if (exc != 0) {
//     return mmclass;
//   }
//   oop addrdict = proc->mmobj()->mm_dictionary_new();
//   oop instance = proc->mmobj()->alloc_instance(proc, mmclass);

//   ((struct addrinfo**) instance)[2] = addrinfo;
//   ((oop*) instance)[3] = addrdict;

// #define addrdict_set(k, v) proc->mmobj()->                              \
//     mm_dictionary_set(proc, addrdict, proc->vm()->new_symbol(k),        \
//                       tag_small_int(v));

//   addrdict_set("ai_flags", addrinfo->ai_flags);
//   addrdict_set("ai_family", addrinfo->ai_family);
//   addrdict_set("ai_socktype", addrinfo->ai_socktype);
//   addrdict_set("ai_protocol", addrinfo->ai_protocol);
// #undef addrdict_set

//   return instance;
// }


// /* -- inet_ntop -- */

// static int prim_net_inet_ntop(Process* proc) {
//   //TYPE_CHECK_ARG(0, "Object");
//   oop addr_oop = proc->get_arg(0);
//   struct sockaddr_storage* addr =
//     (struct sockaddr_storage*) ((oop *) addr_oop)[2];

//   char buffer[INET6_ADDRSTRLEN] = { 0 };
//   switch (addr->ss_family) {
//   case AF_INET:
//     inet_ntop(addr->ss_family,
//               &((struct sockaddr_in *) addr)->sin_addr,
//               buffer, INET_ADDRSTRLEN);
//     break;
//   case AF_INET6:
//     inet_ntop(addr->ss_family,
//               &((struct sockaddr_in6 *) addr)->sin6_addr,
//               buffer, INET6_ADDRSTRLEN);
//     break;
//   }
//   proc->stack_push(proc->mmobj()->mm_string_new(buffer));
//   return 0;
// }

// /* -- close -- */




void net_init_primitives(VM *vm) {
  vm->register_primitive("net_getaddrinfo", prim_net_getaddrinfo);
  vm->register_primitive("net_socket", prim_net_socket);
  vm->register_primitive("net_setsockopt", prim_net_setsockopt);
  vm->register_primitive("net_addrinfo_ai_family", prim_net_addrinfo_ai_family);
  vm->register_primitive("net_addrinfo_ai_socktype", prim_net_addrinfo_ai_socktype);
  vm->register_primitive("net_addrinfo_ai_protocol", prim_net_addrinfo_ai_protocol);
  vm->register_primitive("net_addrinfo_ai_addr", prim_net_addrinfo_ai_addr);
  // vm->register_primitive("net_addrinfo_ai_addrlen", prim_net_addrinfo_ai_addrlen);
  vm->register_primitive("net_bind", prim_net_bind);
  vm->register_primitive("net_listen", prim_net_listen);
  vm->register_primitive("net_accept", prim_net_accept);
  vm->register_primitive("net_close", prim_net_close);
  vm->register_primitive("net_recv", prim_net_recv);
  vm->register_primitive("net_send", prim_net_send);
  vm->register_primitive("net_connect", prim_net_connect);
  // vm->register_primitive("net_inet_ntop", prim_net_inet_ntop);
}
