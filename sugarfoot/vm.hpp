#ifndef VM_HPP
#define VM_HPP

#include <list>
#include <string>

#define STACK_INITIAL_SIZE 1024 * 1024 * 8

class VMProcess;
class VM;

typedef void* oop;


class Image {
public:
  Image(VM*, std::string data);
  oop oop_entry();
  oop oop_imodule();
private:
  oop _entry;
  oop _imod;
};


class VM {
public:
  VM();
  int start(char* filepath);

  oop heap_store(void* data);
private:
  std::list<VMProcess*> _processes;
  std::list<oop> _heap;
};

class VMProcess {
public:
  enum {
    STATE_RUNNING,
    STATE_PAUSED
  } ProcessState;
  typedef char* call_stack_t;

  VMProcess(VM*);
  int run_image(std::string);
  //int run_module(oop);

  void push_activation_record();
  void pop_activation_record();
  void init_activation_record(oop, oop, oop, oop);
  oop send(oop, oop, oop, oop);
private:

  VM* _vm;
  int _state;
  call_stack_t* _call_stack;

  // registers
  oop _mp;
  oop _cp;
  oop _rp;
  oop _rdp;
  void* _ep;
  void* _ip;
};

#endif
