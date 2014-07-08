#define  WSIZE 8

#if WSIZE == 4
  typedef int word;
  typedef int number;
#elif WSIZE == 8
  typedef long word;
  typedef long number;
#endif

typedef char byte;
typedef word* oop;

typedef int bytecode;

class Process;
typedef oop (*prim_function_t) (Process*);

#define DEFAULT_STACK_SIZE sizeof(word) * 1024 * 10

// bytecodes

#define PUSH_LITERAL 3
#define PUSH_LOCAL 2
#define PUSH_MODULE 7

#define POP_LOCAL 21

#define RETURN_TOP 31
#define RETURN_THIS 30

#define SEND 40
